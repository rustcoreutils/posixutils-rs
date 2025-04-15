//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, BufRead};
use std::path::PathBuf;

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, setlocale, textdomain};
use plib::io::input_stream_opt;
use topological_sort::TopologicalSort;

/// tsort - topological sort
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// File to read as input.
    file: Option<PathBuf>,
}

fn tsort_file(pathname: &Option<PathBuf>) -> io::Result<()> {
    let file = input_stream_opt(pathname)?;
    let mut reader = io::BufReader::new(file);

    let mut ts = TopologicalSort::<String>::new();
    let mut sv: Vec<String> = Vec::new();

    loop {
        let mut buffer = String::new();
        let n_read = reader.read_line(&mut buffer)?;
        if n_read == 0 {
            break;
        }

        for token in buffer.split_whitespace() {
            sv.push(String::from(token));

            if sv.len() == 2 {
                if sv[0] == sv[1] {
                    ts.insert(String::from(&sv[0]));
                } else {
                    ts.add_dependency(String::from(&sv[0]), String::from(&sv[1]));
                }
                sv.clear();
            }
        }
    }

    for s in ts {
        println!("{}", s);
    }

    Ok(())
}

fn pathname_display(path: &Option<PathBuf>) -> String {
    match path {
        None => String::from("stdin"),
        Some(p) => p.display().to_string(),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    if let Err(e) = tsort_file(&args.file) {
        exit_code = 1;
        eprintln!("{}: {}", pathname_display(&args.file), e);
    }

    std::process::exit(exit_code)
}
