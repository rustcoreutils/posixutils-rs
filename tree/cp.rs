//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod common;

use self::common::{copy_file, copy_files, error_string, CopyConfig, DerefMode};
use clap::Parser;
use gettextrs::gettext;
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

    // No `default_value_t`: `overrides_with_all` resolves which flag was given last, but it does
    // not clear a value clap inserted as a default, which is what left `-P` with no effect.
    #[arg(
        short = 'L',
        long,
        overrides_with_all = [
            "follow_cli",
            "dereference",
            "no_dereference"
        ],
        requires = "recursive",
        help = gettext("Follow symlinks in source")
    )]
    dereference: bool,

    // Unlike -H and -L this carries no `requires = "recursive"`: the cp synopsis (POSIX 90580)
    // allows -P in all three forms.
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

/// Resolves which of -H, -L and -P is in effect.
///
/// `overrides_with_all` leaves at most one of the three set, so this is a straight mapping; the
/// interesting part is the default. Without -R, POSIX 90610-90612 requires acting on what a link
/// refers to. With -R and none of the three given, 90614-90615 leaves it unspecified, and both
/// GNU and the BSDs behave as -P -- which is also the only choice that keeps a recursive copy
/// inside the tree it was pointed at.
fn deref_mode(args: &Args) -> DerefMode {
    if args.no_dereference {
        DerefMode::Never
    } else if args.dereference {
        DerefMode::Always
    } else if args.follow_cli {
        DerefMode::CommandLineOnly
    } else if args.recursive {
        DerefMode::Never
    } else {
        DerefMode::Always
    }
}

impl CopyConfig {
    fn new(args: &Args) -> Self {
        CopyConfig {
            force: args.force,
            deref: deref_mode(args),
            interactive: args.interactive,
            preserve: args.preserve,
            recursive: args.recursive,
            prog: "cp",
            // POSIX cp continues with same-level/ancestor files after a per-file failure.
            continue_on_error: true,
        }
    }
}

fn prompt_user(prompt: &str) -> bool {
    eprint!("cp: {prompt} ");
    let mut response = String::new();
    // A read error or EOF is a non-affirmative response, not a panic.
    if io::stdin().read_line(&mut response).unwrap_or(0) == 0 {
        return false;
    }
    plib::locale::is_affirmative(response.trim_end_matches(['\r', '\n']))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("cp");

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

    // POSIX cp DESCRIPTION (90605-90606): "It shall be an error if target does not exist and more
    // than two operands are specified, or if target exists and does not name a directory." Both
    // map to: more than one source with a target that is not an existing directory.
    if !dir_exists && sources.len() > 1 {
        eprintln!(
            "cp: {}",
            gettext!("target '{}' is not a directory", target.display())
        );
        std::process::exit(1);
    }

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
                // `copy_file` already emitted its per-file diagnostics (empty-message marker).
                let s = error_string(&e);
                if !s.is_empty() {
                    eprintln!("cp: {s}");
                }
                std::process::exit(1);
            }
        }
    }
}
