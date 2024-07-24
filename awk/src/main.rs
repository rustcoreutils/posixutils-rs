//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::error::Error;

mod compiler;
mod format;
mod interpreter;
mod program;
mod regex;

/// awk - pattern scanning and processing language
#[derive(Debug, Parser)]
struct Args {
    /// Define the input field separator
    #[arg(short = 'F')]
    separator_string: Option<String>,

    /// Specify the program files
    #[arg(short = 'f', action = clap::ArgAction::Append, required_unless_present = "program", conflicts_with = "program")]
    program_files: Vec<String>,

    /// Globals assignments, executed before the start of the program
    #[arg(short = 'v', action = clap::ArgAction::Append)]
    assignments: Vec<String>,

    #[arg(
        required_unless_present = "program_files",
        conflicts_with = "program_files"
    )]
    program: Option<String>,

    arguments: Vec<String>,
}

fn main() -> Result<(), Box<dyn Error>> {
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let args = Args::parse();
    Ok(())
}
