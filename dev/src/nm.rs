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

use clap::{Parser, ValueEnum};
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::io::{self, Error, ErrorKind};

#[derive(Debug, ValueEnum, Clone)]
enum OutputType {
    D,
    O,
    X,
}

/// nm - write the name list of an object file
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Write the full pathname or library name of an object on each line.
    #[arg(short = 'A', long)]
    print_name: bool,

    /// Write only external (global) and static symbol information.
    #[arg(short = 'e', long = "external")]
    external_only: bool,

    /// Produce full output.
    #[arg(short, long)]
    full: bool,

    /// Write only external (global) symbol information.
    #[arg(short, long)]
    global: bool,

    /// Write numeric values in octal (equivalent to -t o).
    #[arg(short, long)]
    octal: bool,

    /// Write numeric values in hexadecimal (equivalent to -t x).
    #[arg(short = 'x', long)]
    hex: bool,

    /// Write information in a portable output format
    #[arg(short = 'P', long)]
    portable: bool,

    /// Write each numeric value in the specified format.
    #[arg(short = 't', long = "format", value_enum)]
    out_type: OutputType,

    /// Write only undefined symbols.
    #[arg(short, long)]
    undef: bool,

    /// Sort output by value instead of by symbol name.
    #[arg(short, long)]
    value_sort: bool,

    /// Input object file
    file: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    Ok(())
}
