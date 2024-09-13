//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::cmp;
use std::fs::{File, OpenOptions};
use std::io::{self, BufRead, Error, ErrorKind, Read, Write};
use std::path::PathBuf;

/// split - split a file into pieces
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Use suffix_length letters to form the suffix portion of the filenames of the split file.
    #[arg(short='a', long, default_value_t=2, value_parser = clap::value_parser!(u32).range(1..))]
    suffix_len: u32,

    /// Use suffix_length letters to form the suffix portion of the filenames of the split file.
    #[arg(short, long, group = "mode", value_parser = clap::value_parser!(u64).range(1..))]
    lines: Option<u64>,

    /// Split a file into pieces n bytes in size.
    #[arg(short, long, group = "mode")]
    bytes: Option<String>,

    /// File to be split
    #[arg(default_value = "")]
    file: PathBuf,

    /// Prefix of output files
    #[arg(default_value = "x")]
    prefix: String,
}

fn inc_char(ch: char) -> char {
    ((ch as u8) + 1) as char
}

struct OutputState {
    prefix: String,
    boundary: u64,

    suffix: String,
    suffix_len: u32,
    count: u64,
    outf: Option<File>,
}

impl OutputState {
    fn new(prefix: &str, boundary: u64, suffix_len: u32) -> OutputState {
        OutputState {
            prefix: String::from(prefix),
            boundary,
            suffix_len,
            suffix: String::new(),
            count: 0,
            outf: None,
        }
    }

    fn incr_suffix(&mut self) -> Result<(), &'static str> {
        assert!(self.suffix_len > 1);

        if self.suffix.is_empty() {
            self.suffix = "a".repeat(self.suffix_len as usize);
            return Ok(());
        }

        assert!(self.suffix.len() > 1);
        let mut i = self.suffix.len() - 1;
        loop {
            let ch = self.suffix.chars().nth(i).unwrap();
            if ch != 'z' {
                self.suffix
                    .replace_range(i..i + 1, inc_char(ch).to_string().as_str());
                return Ok(());
            }

            self.suffix
                .replace_range(i..i + 1, 'a'.to_string().as_str());

            if i == 0 {
                break;
            }
            i = i - 1;
        }

        Err("maximum suffix reached")
    }

    fn open_output(&mut self) -> io::Result<()> {
        if self.outf.is_some() {
            return Ok(());
        }

        let inc_res = self.incr_suffix();
        if let Err(e) = inc_res {
            return Err(Error::new(ErrorKind::Other, e));
        }

        let out_fn = format!("{}{}", self.prefix, self.suffix);
        let f = OpenOptions::new()
            .read(false)
            .write(true)
            .create(true)
            .truncate(true)
            .open(&out_fn)?;
        self.outf = Some(f);

        Ok(())
    }

    fn close_output(&mut self) {
        if self.outf.is_some() {
            self.outf = None;
            self.count = 0;
        }
    }

    fn incr_output(&mut self, n: u64) {
        self.count = self.count + n;
        assert!(self.count <= self.boundary);

        if self.count == self.boundary {
            self.close_output();
        }
    }

    fn write(&mut self, buf: &[u8]) -> io::Result<()> {
        match &mut self.outf {
            None => {
                // TODO
                #[allow(clippy::assertions_on_constants)]
                {
                    assert!(false);
                }
                Ok(())
            }
            Some(ref mut f) => f.write_all(buf),
        }
    }

    fn output_bytes(&mut self, buf: &[u8]) -> io::Result<()> {
        let mut consumed: usize = 0;
        while consumed < buf.len() {
            self.open_output()?;

            let remainder = buf.len() - consumed;
            let dist = self.boundary - self.count;
            let wlen = cmp::min(dist as usize, remainder);
            let slice = &buf[consumed..consumed + wlen];
            self.write(slice)?;

            consumed = consumed + wlen;

            self.incr_output(wlen as u64);
        }

        Ok(())
    }
}

fn split_by_bytes(args: &Args, bytesplit: String) -> io::Result<()> {
    let mul: u64 = {
        if bytesplit.ends_with("k") {
            1024
        } else if bytesplit.ends_with("m") {
            1024 * 1024
        } else if bytesplit.ends_with("g") {
            1024 * 1024 * 1024
        } else {
            1
        }
    };
    let bytestr = match mul {
        1 => &bytesplit[..],
        _ => &bytesplit[0..bytesplit.len() - 1],
    };
    let boundary: u64 = match bytestr.parse::<u64>() {
        Ok(n) => n * mul,
        Err(e) => {
            eprintln!("{}", e);
            return Err(Error::new(ErrorKind::Other, "invalid byte spec"));
        }
    };

    // open file, or stdin
    let mut file = plib::io::input_stream(&args.file, false)?;
    let mut raw_buffer = [0; plib::BUFSZ];
    let mut state = OutputState::new(&args.prefix, boundary, args.suffix_len);

    loop {
        // read a chunk of file data
        let n_read = file.read(&mut raw_buffer[..])?;
        if n_read == 0 {
            break;
        }

        // slice of buffer containing file data
        let buf = &raw_buffer[0..n_read];

        state.output_bytes(buf)?;
    }

    Ok(())
}

fn split_by_lines(args: &Args, linesplit: u64) -> io::Result<()> {
    assert!(linesplit > 0);

    // open file, or stdin
    let mut reader = plib::io::input_reader(&args.file, false)?;
    let mut state = OutputState::new(&args.prefix, linesplit, args.suffix_len);

    loop {
        let mut buffer = String::new();
        let n_read = reader.read_line(&mut buffer)?;
        if n_read == 0 {
            break;
        }

        state.open_output()?;

        state.write(buffer.as_ref())?;

        state.incr_output(1);
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let mut args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    if args.lines.is_none() && args.bytes.is_none() {
        args.lines = Some(1000);
    }

    if args.lines.is_some() {
        split_by_lines(&args, args.lines.unwrap())?;
    } else {
        split_by_bytes(&args, args.bytes.clone().unwrap())?;
    }

    Ok(())
}
