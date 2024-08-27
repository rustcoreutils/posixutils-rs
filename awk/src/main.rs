//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::compiler::compile_program;
use crate::interpreter::interpret;
use crate::program::Program;
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::error::Error;
use std::io::Read;

mod compiler;
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
    #[arg(short = 'f', action = clap::ArgAction::Append)]
    program_files: Vec<String>,

    /// Globals assignments, executed before the start of the program
    #[arg(short = 'v', action = clap::ArgAction::Append)]
    assignments: Vec<String>,

    arguments: Vec<String>,
}

fn compile_soruces(args: &Args) -> Result<Program, String> {
    if args.program_files.len() > 0 {
        let mut combined_sources = String::new();
        for source_file in &args.program_files {
            let mut file = std::fs::File::open(source_file)
                .map_err(|_| format!("could not open file '{}'", source_file))?;
            file.read_to_string(&mut combined_sources)
                .map_err(|_| format!("could not read file '{}'", source_file))?;
        }
        compile_program(&combined_sources).map_err(|err| format!("{}", err))
    } else if args.arguments.len() > 0 {
        compile_program(&args.arguments[0]).map_err(|err| format!("{}", err))
    } else {
        Err("missing program argument".to_string())
    }
}

fn exit_if_error<T>(r: Result<T, String>) -> T {
    match r {
        Ok(v) => v,
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let args = Args::parse();

    let program = exit_if_error(compile_soruces(&args));

    let result = exit_if_error(interpret(program, args.arguments));
    std::process::exit(result);
}
