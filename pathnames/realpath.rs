//
// Copyright (c) 2024 Ian McLinden
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::path::{Component, Path, PathBuf};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

/// realpath - return resolved canonical path
#[derive(Parser)]
#[command(version, about = gettext("realpath - return resolved canonical path"))]
struct Args {
    #[arg(short = 'e', overrides_with = "_canonicalize_missing", help = gettext("Error if the path cannot be resolved"))]
    canonicalize_existing: bool,

    #[arg(short = 'E', overrides_with = "canonicalize_existing", help = gettext("Do not error if the path cannot be resolved (default)"))]
    _canonicalize_missing: bool,

    #[arg(short, long, help = gettext("Don't print errors when paths cannot be resolved"))]
    quiet: bool,

    #[arg(value_name = "PATH", default_value = ".", help = gettext("Paths to resolve"))]
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
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

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
