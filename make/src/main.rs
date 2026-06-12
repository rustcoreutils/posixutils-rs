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
    rule::KEEP_GOING_ERROR,
    Make,
};

const MAKEFILE_NAME: [&str; 2] = ["makefile", "Makefile"];
const MAKEFILE_PATH: [&str; 2] = [
    formatcp!("./{}", MAKEFILE_NAME[0]),
    formatcp!("./{}", MAKEFILE_NAME[1]),
];

/// make - maintain, update, and regenerate groups of programs
// todo: sort arguments
#[derive(Parser, Debug)]
#[command(version, about = gettext("make - maintain, update, and regenerate groups of programs"))]
struct Args {
    #[arg(short = 'C', long, help = "Change to DIRECTORY before doing anything")]
    directory: Option<PathBuf>,

    #[arg(
        short = 'S',
        long,
        help = "Terminate make if error occurs. Default behavior"
    )]
    terminate: bool,

    #[arg(
        short = 'f',
        long,
        help = "Path to a makefile to parse (may be given multiple times; processed in order)"
    )]
    makefile: Vec<PathBuf>,

    #[arg(
        short = 'j',
        long,
        value_name = "maxjobs",
        overrides_with = "jobs",
        help = "Maximum number of targets to update concurrently (last value wins)"
    )]
    jobs: Option<usize>,

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

fn print_rules(rules: &BTreeMap<String, BTreeSet<String>>) {
    print!("{:?}", rules);
}

/// The `-k` "target could not be remade" diagnostic, routed through `gettext`
/// so `LC_MESSAGES` can translate it (the English msgids reproduce the original
/// wording verbatim).
fn target_not_remade(target: &str) -> String {
    format!(
        "{}: {} {} {}",
        gettext("make"),
        gettext("Target"),
        target,
        gettext("not remade because of errors")
    )
}

/// Build the effective argument vector, seeding options from the `MAKEFLAGS`
/// environment variable ahead of the real command line (POSIX). The letters-only
/// first word (e.g. `MAKEFLAGS=kn`) becomes a combined short option (`-kn`);
/// words containing `=` are macro operands; words already starting with `-` are
/// passed verbatim. `MAKEFLAGS` is inherited by recipe sub-processes via the
/// environment, so it propagates to sub-makes.
fn args_with_makeflags() -> Vec<OsString> {
    let mut args: Vec<OsString> = env::args_os().collect();
    let Ok(flags) = env::var("MAKEFLAGS") else {
        return args;
    };
    let flags = flags.trim();
    if flags.is_empty() {
        return args;
    }

    let mut injected: Vec<OsString> = Vec::new();
    for (i, word) in flags.split_whitespace().enumerate() {
        if i == 0 && !word.starts_with('-') && !word.contains('=') {
            // Letters-only form: `kn` -> `-kn`.
            injected.push(OsString::from(format!("-{word}")));
        } else {
            injected.push(OsString::from(word));
        }
    }

    // Insert the seeded options just after argv[0], before the real arguments.
    let tail = args.split_off(1);
    args.extend(injected);
    args.extend(tail);
    args
}

/// Read one makefile operand into a string, honoring `-` as standard input.
fn read_makefile(path: &Path) -> Result<String, ErrorCode> {
    if path == Path::new("-") {
        read_stdin()
    } else {
        fs::read_to_string(path).map_err(|err| IoError(err.kind()))
    }
}

/// Append `part` to `contents`, inserting a separating newline only between
/// non-empty parts. A trailing newline is never added, so a single empty
/// makefile stays empty (which the parser reports as "No targets").
fn append_part(contents: &mut String, part: &str) {
    if !contents.is_empty() && !contents.ends_with('\n') {
        contents.push('\n');
    }
    contents.push_str(part);
}

/// Parse the makefile(s). Multiple `-f` operands are concatenated in the order
/// given (POSIX); with none, the first default makefile found is used.
/// Command-line `macro=value` operands are appended last so they take
/// precedence over definitions in the makefile(s).
fn parse_makefile(paths: &[PathBuf], cmdline_macros: &[String]) -> Result<Makefile, ErrorCode> {
    let mut contents = String::new();

    if paths.is_empty() {
        let default = MAKEFILE_PATH
            .iter()
            .map(Path::new)
            .find(|p| p.exists())
            .ok_or(NoMakefile)?;
        append_part(&mut contents, &read_makefile(default)?);
    } else {
        for path in paths {
            append_part(&mut contents, &read_makefile(path)?);
        }
    }

    for macro_def in cmdline_macros {
        append_part(&mut contents, macro_def);
    }

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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

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
        jobs,
        mut targets,
    } = Args::parse_from(args_with_makeflags());

    let mut status_code = 0;

    // -j maxjobs: a non-positive value is unspecified; treat it (and absence)
    // as 1 (no parallelism).
    let jobs = jobs.filter(|&j| j >= 1).unwrap_or(1);

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
        jobs,
        terminate,
        ..Default::default()
    };

    if clear {
        config.rules.clear();
        config.suffixes.clear();
    }

    ENV_MACROS.store(env_macros, Relaxed);

    // Separate command-line `macro=value` operands from target operands
    // (POSIX SYNOPSIS allows them to be intermixed).
    let mut cmdline_macros: Vec<String> = Vec::new();
    targets.retain(|operand| {
        let operand = operand.to_string_lossy();
        if posixutils_make::parser::preprocessor::is_macro_definition(&operand) {
            cmdline_macros.push(operand.into_owned());
            false
        } else {
            true
        }
    });

    let parsed = match parse_makefile(&makefile, &cmdline_macros) {
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
        KEEP_GOING_ERROR.store(false, Relaxed);
        match make.build_target(&target) {
            Ok(updated) => {
                // Under `-k` a failed recipe is reported and swallowed so the
                // build can continue; the flag tells us this target could not
                // actually be remade.
                if KEEP_GOING_ERROR.load(Relaxed) {
                    eprintln!("{}", target_not_remade(&target));
                    had_error = true;
                } else if !updated {
                    println!(
                        "{}: `{}` {}",
                        gettext("make"),
                        target,
                        gettext("is up to date.")
                    );
                }
            }
            Err(err) => {
                eprintln!("make: {}", err);
                had_error = true;
                if keep_going {
                    // `-k`: report this target, then keep building the
                    // remaining (independent) command-line targets.
                    eprintln!("{}", target_not_remade(&target));
                } else {
                    status_code = err.into();
                    break;
                }
            }
        }
    }

    // Any non-ignored build failure must exit with a status > 1.
    if had_error && status_code == 0 {
        status_code = 2;
    }
    process::exit(status_code);
}
