//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use core::str::FromStr;
use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsString;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering::Relaxed;
use std::{env, fs, io, process};

use clap::Parser;
use const_format::formatcp;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

use posixutils_make::{
    config::Config,
    error_code::ErrorCode::{self, *},
    parser::{preprocessor::ENV_MACROS, Makefile},
    Make,
};

const MAKEFILE_NAME: [&str; 2] = ["makefile", "Makefile"];
const MAKEFILE_PATH: [&str; 2] = [
    formatcp!("./{}", MAKEFILE_NAME[0]),
    formatcp!("./{}", MAKEFILE_NAME[1]),
];

// todo: sort arguments
#[derive(Parser, Debug)]
struct Args {
    #[arg(short = 'C', long, help = "Change to DIRECTORY before doing anything")]
    directory: Option<PathBuf>,

    #[arg(
        short = 'S',
        long,
        help = "Terminate make if error occurs. Default behavior"
    )]
    terminate: bool,

    #[arg(short = 'f', long, help = "Path to the makefile to parse")]
    makefile: Option<PathBuf>,

    #[arg(short = 'i', long, help = "Ignore errors in the recipe")]
    ignore: bool,

    #[arg(
        short = 'e',
        long,
        help = "Cause environment variables to override macro assignments within makefiles"
    )]
    env_macros: bool,

    #[arg(
        short = 'n',
        long,
        help = "Print commands to stdout and do not execute them"
    )]
    dry_run: bool,

    #[arg(short = 's', long, help = "Do not print recipe lines")]
    silent: bool,

    #[arg(
        short = 'q',
        long,
        help = "Return a zero exit value if the target file is up-to-date; otherwise, return an exit value of 1."
    )]
    quit: bool,

    #[arg(
        short = 'k',
        long,
        help = "Continue to update other targets that do not depend on the current target if a non-ignored error occur"
    )]
    keep_going: bool,

    #[arg(
        short = 'r',
        long,
        help = "Clear the suffix list and do not use the built-in rules"
    )]
    clear: bool,

    #[arg(
        short = 'p',
        long,
        help = "Write to standard output the complete set of macro definitions and target descriptions."
    )]
    print: bool,

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
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let Args {
        directory,
        makefile,
        env_macros,
        ignore,
        dry_run,
        silent,
        quit,
        clear,
        touch,
        print,
        terminate,
        keep_going,
        mut targets,
    } = Args::parse();

    let mut status_code = 0;

    // -C flag
    if let Some(dir) = directory {
        env::set_current_dir(dir)?;
    }

    let mut config = Config {
        ignore,
        dry_run,
        silent,
        touch,
        env_macros,
        quit,
        keep_going,
        clear,
        print,
        precious: false,
        terminate,
        ..Default::default()
    };

    if clear {
        config.rules.clear();
    }

    ENV_MACROS.store(env_macros, Relaxed);

    let parsed = match parse_makefile(makefile.as_ref()) {
        Ok(parsed) => parsed,
        Err(err) => {
            // -p flag
            if print {
                // If makefile is not provided or parsing failed, print the default rules
                print_rules(&config.rules);
                return Ok(());
            } else {
                eprintln!("make: {}", err);
                process::exit(err.into());
            }
        }
    };

    let make = Make::try_from((parsed, config)).unwrap_or_else(|err| {
        eprintln!("make: {err}");
        process::exit(err.into());
    });

    // -p flag
    if print {
        // Call print for  global config rules
        print_rules(&make.config.rules);
    }

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

    let mut had_error = false;
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

        if keep_going {
            eprintln!(
                "{}: Target {} not remade because of errors",
                gettext("make"),
                target
            );
            had_error = true;
        }

        if status_code != 0 {
            break;
        }
    }

    if had_error {
        status_code = 2;
    }
    process::exit(status_code);
}

fn print_rules(rules: &BTreeMap<String, BTreeSet<String>>) {
    print!("{:?}", rules);
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
        Err(err) => Err(ErrorCode::ParserError { constraint: err }),
    }
}

/// Reads the makefile from `stdin` until EOF (Ctrl + D)
fn read_stdin() -> Result<String, ErrorCode> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    Ok(buffer)
}
