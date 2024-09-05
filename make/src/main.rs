//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use core::str::FromStr;
use std::{
    env,
    ffi::OsString,
    fs,
    io::{self, Read},
    path::{Path, PathBuf},
    process,
};

use clap::Parser;
use const_format::formatcp;
use gettextrs::{bind_textdomain_codeset, textdomain};
use makefile_lossless::Makefile;
use plib::PROJECT_NAME;
use posixutils_make::{
    config::Config,
    error_code::ErrorCode::{self, *},
    Make,
};

const MAKEFILE_NAME: [&str; 2] = ["makefile", "Makefile"];
const MAKEFILE_PATH: [&str; 2] = [
    formatcp!("./{}", MAKEFILE_NAME[0]),
    formatcp!("./{}", MAKEFILE_NAME[1]),
];

#[derive(Parser, Debug)]
struct Args {
    #[arg(short = 'C', long, help = "Change to DIRECTORY before doing anything")]
    directory: Option<PathBuf>,

    #[arg(short = 'f', long, help = "Path to the makefile to parse")]
    makefile: Option<PathBuf>,

    #[arg(short = 'i', long, help = "Ignore errors in the recipe")]
    ignore: bool,

    #[arg(
        short = 'n',
        long,
        help = "Print commands to stdout and do not execute them"
    )]
    dry_run: bool,

    #[arg(short = 's', long, help = "Do not print recipe lines")]
    silent: bool,

    #[arg(
        short = 't',
        long,
        help = "If makefile should touch targets on execution"
    )]
    touch: bool,

    #[arg(help = "Targets to build")]
    targets: Vec<OsString>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let Args {
        directory,
        makefile,
        ignore,
        dry_run,
        silent,
        touch,
        mut targets,
    } = Args::parse();

    let mut status_code = 0;

    // -C flag
    if let Some(dir) = directory {
        env::set_current_dir(dir)?;
    }

    let parsed = parse_makefile(makefile.as_ref()).unwrap_or_else(|err| {
        eprintln!("make: {}", err);
        process::exit(err.into());
    });
    let config = Config {
        ignore,
        dry_run,
        silent,
        touch,
    };

    let make = Make::try_from((parsed, config)).unwrap_or_else(|err| {
        eprintln!("make: {err}");
        process::exit(err.into());
    });

    if targets.is_empty() {
        let target = make
            .first_target()
            .unwrap_or_else(|err| {
                eprintln!("make: {err}");
                process::exit(err.into());
            })
            .to_string()
            .into();

        targets.push(target);
    }

    for target in targets {
        let target = target.into_string().unwrap();
        match make.build_target(&target) {
            Ok(updated) => {
                if !updated {
                    println!("make: `{target}` is up to date.");
                }
            }
            Err(err) => {
                eprintln!("make: {}", err);
                status_code = err.into();
            }
        }

        if status_code != 0 {
            break;
        }
    }

    process::exit(status_code);
}

/// Parse the makefile at the given path, or the first default makefile found.
/// If no makefile is found, print an error message and exit.
fn parse_makefile(path: Option<impl AsRef<Path>>) -> Result<Makefile, ErrorCode> {
    let path = path.as_ref().map(|p| p.as_ref());

    let path = match path {
        Some(path) => path,
        None => {
            let mut makefile = None;
            for m in MAKEFILE_PATH.iter() {
                let path = Path::new(m);
                if path.exists() {
                    makefile = Some(path);
                    break;
                }
            }
            if let Some(makefile) = makefile {
                makefile
            } else {
                return Err(NoMakefile);
            }
        }
    };

    let contents = if path == Path::new("-") {
        read_stdin()?
    } else {
        match fs::read_to_string(path) {
            Ok(contents) => contents,
            Err(err) => {
                return Err(IoError(err.kind()));
            }
        }
    };

    match Makefile::from_str(&contents) {
        Ok(makefile) => Ok(makefile),
        Err(err) => Err(ParseError(err.to_string())),
    }
}

/// Reads the makefile from `stdin` until EOF (Ctrl + D)
fn read_stdin() -> Result<String, ErrorCode> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    Ok(buffer)
}
