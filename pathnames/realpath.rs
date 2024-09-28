//
// Copyright (c) 2024 Ian McLinden
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::path::{Component, Path, PathBuf};

/// realpath -- return resolved canonical path
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Error if the path cannot be resolved
    #[arg(short = 'e', overrides_with = "_canonicalize_missing")]
    canonicalize_existing: bool,

    /// Do not error if the path cannot be resolved (default)
    #[arg(short = 'E', overrides_with = "canonicalize_existing")]
    _canonicalize_missing: bool,

    /// Don't print errors when paths cannot be resolved
    #[arg(short, long)]
    quiet: bool,

    #[arg(value_name = "PATH", default_value = ".")]
    paths: Vec<PathBuf>,
}

/// Returns a normalized path.
/// If `must_exist`, returns an error if the path cannot be resolved
fn normalize<P: AsRef<Path>>(path: P) -> std::io::Result<PathBuf> {
    let mut out = PathBuf::new();

    let abs_path = if path.as_ref().is_absolute() {
        path.as_ref()
    } else {
        &std::env::current_dir()?.join(path)
    };

    // from cargo/src/cargo/util/paths.rs
    for component in abs_path.components() {
        match component {
            Component::Prefix(..) => unreachable!(),
            Component::RootDir => {
                out.push(component);
            }
            Component::CurDir => {}
            Component::ParentDir => {
                out.pop();
            }
            Component::Normal(c) => {
                out.push(c);
            }
        }
    }
    Ok(out)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let mut exit_code = 0;

    for path in args.paths {
        let ret = if args.canonicalize_existing {
            std::fs::canonicalize(&path)
        } else {
            normalize(&path)
        };

        match ret {
            // Could also std::io::stdout().write_all(p.as_os_str().as_bytes())
            // if non-utf-8 compatability is an issue
            Ok(p) => println!("{}", p.to_string_lossy()),
            Err(e) => {
                if !args.quiet {
                    eprintln!(
                        "realpath: {}: {}",
                        path.to_string_lossy(),
                        gettext(e.to_string())
                    );
                }
                exit_code |= 1;
            }
        }
    }

    std::process::exit(exit_code);
}
