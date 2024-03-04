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

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::fs;
use std::io::{self, BufRead, Read};
use topological_sort::TopologicalSort;

/// tsort - topological sort
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// File to read as input.
    file: Option<String>,
}

fn tsort_file(filename: &str) -> io::Result<()> {
    let file: Box<dyn Read>;
    if filename == "" {
        file = Box::new(io::stdin().lock());
    } else {
        file = Box::new(fs::File::open(filename)?);
    }

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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    let filename = match &args.file {
        None => String::new(),
        Some(name) => String::from(name),
    };

    match tsort_file(&filename) {
        Ok(()) => {}
        Err(e) => {
            exit_code = 1;
            eprintln!("{}: {}", filename, e);
        }
    }

    std::process::exit(exit_code)
}
