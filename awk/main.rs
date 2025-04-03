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
use clap::Parser;
use compiler::SourceFile;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::error::Error;
use std::fmt::Display;
use std::io::Read;

mod compiler;
mod interpreter;
mod program;
mod regex;

#[derive(Parser)]
#[command(version, about = gettext("awk - pattern scanning and processing language"))]
struct Args {
    #[arg(short = 'F', help = gettext("Define the input field separator"))]
    separator_string: Option<String>,

    #[arg(
        short = 'f',
        action = clap::ArgAction::Append,
        help = gettext("Specify the program files")
    )]
    program_files: Vec<String>,

    #[arg(
        short = 'v',
        action = clap::ArgAction::Append,
        help = gettext("Globals assignments, executed before the start of the program")
    )]
    assignments: Vec<String>,

    arguments: Vec<String>,
}

fn exit_if_error<T, U: Display>(r: Result<T, U>) -> T {
    match r {
        Ok(v) => v,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let return_status = if !args.program_files.is_empty() {
        let mut sources = Vec::new();
        for source_file in &args.program_files {
            let mut file = std::fs::File::open(source_file)
                .map_err(|_| gettext!("could not open file '{}'", source_file))?;
            let mut contents = String::new();
            file.read_to_string(&mut contents)
                .map_err(|_| gettext!("could not read file '{}'", source_file))?;
            sources.push(SourceFile {
                contents,
                filename: source_file.clone(),
            });
        }
        let program = exit_if_error(compile_program(&sources));
        exit_if_error(interpret(
            program,
            &args.arguments,
            &args.assignments,
            args.separator_string,
        ))
    } else if !args.arguments.is_empty() {
        let program = exit_if_error(compile_program(&[SourceFile::stdin(
            args.arguments[0].clone(),
        )]));
        exit_if_error(interpret(
            program,
            &args.arguments[1..],
            &args.assignments,
            args.separator_string,
        ))
    } else {
        eprintln!("{}", gettext("missing program argument"));
        1
    };
    std::process::exit(return_status);
}
