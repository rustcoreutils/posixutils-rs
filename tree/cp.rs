//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod common;

use self::common::{CopyConfig, copy_file, copy_files, error_string};
use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use std::collections::HashSet;
use std::path::PathBuf;
use std::{fs, io};

/// cp - copy files
#[derive(Parser)]
#[command(version, about = gettext("cp - copy files"))]
struct Args {
    #[arg(short, long, help = gettext("Do not prompt for confirmation if the destination path exists"))]
    force: bool,

    #[arg(
        short = 'H',
        long,
        overrides_with_all = [
            "follow_cli",
            "dereference",
            "no_dereference"
        ],
        requires = "recursive",
        help = gettext("Follow command line symlinks")
    )]
    follow_cli: bool,

    #[arg(
        short = 'L',
        long,
        overrides_with_all = [
            "follow_cli",
            "dereference",
            "no_dereference"
        ],
        requires = "recursive",
        default_value_t = true,
        help = gettext("Follow symlinks in source")
    )]
    dereference: bool,

    #[arg(
        short = 'P',
        long,
        overrides_with_all = [
            "follow_cli",
            "dereference",
            "no_dereference"
        ],
        help = gettext("Never follow symlinks in source")
    )]
    no_dereference: bool,

    #[arg(short, long, help = gettext("Prompt for confirmation if the destination path exists"))]
    interactive: bool,

    #[arg(short, long, help = gettext("Duplicate the characteristics of each source file in the corresponding destination file"))]
    preserve: bool,

    #[arg(short = 'R', visible_short_alias = 'r', long, help = gettext("Copy file hierarchies"))]
    recursive: bool,

    #[arg(help = gettext("Source(s) and target of move(s)"))]
    files: Vec<PathBuf>,
}

impl CopyConfig {
    fn new(args: &Args) -> Self {
        // `args.no_dereference` serves only to disable `args.dereference` or
        // `follow_cli`
        CopyConfig {
            force: args.force,
            follow_cli: args.follow_cli,
            dereference: args.dereference,
            interactive: args.interactive,
            preserve: args.preserve,
            recursive: args.recursive,
        }
    }
}

fn prompt_user(prompt: &str) -> bool {
    eprint!("cp: {} ", prompt);
    let mut response = String::new();
    io::stdin().read_line(&mut response).unwrap();
    response.to_lowercase().starts_with('y')
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    if args.files.len() < 2 {
        eprintln!("{}", gettext("Must supply a source and target for copy"));
        std::process::exit(1);
    }

    // split sources and target
    let sources = &args.files[0..args.files.len() - 1];
    let target = &args.files[args.files.len() - 1];

    // choose mode based on whether target is a directory
    let dir_exists = {
        match fs::metadata(target) {
            Ok(md) => md.is_dir(),
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    false
                } else {
                    let err_str =
                        gettext!("cannot stat '{}': {}", target.display(), error_string(&e));
                    eprintln!("cp: {}", err_str);
                    std::process::exit(1);
                }
            }
        }
    };

    let cfg = CopyConfig::new(&args);
    if dir_exists {
        match copy_files(&cfg, sources, target, None, prompt_user) {
            Some(_) => Ok(()),
            None => std::process::exit(1),
        }
    } else {
        let mut created_files = HashSet::new();

        match copy_file(
            &cfg,
            &sources[0],
            target,
            &mut created_files,
            None,
            prompt_user,
        ) {
            Ok(_) => Ok(()),
            Err(e) => {
                eprintln!("cp: {}", error_string(&e));
                std::process::exit(1);
            }
        }
    }
}
