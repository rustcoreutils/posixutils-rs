//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::ffi::CString;
use std::path::{Component, Path};

const _POSIX_PATH_MAX: usize = 255;
const _POSIX_NAME_MAX: usize = 14;

#[derive(Parser)]
#[command(version, about = gettext("pathchk - check pathnames"))]
struct Args {
    #[arg(
        short,
        long,
        group = "mode",
        help = gettext(
            "Instead of performing checks based on the underlying file system, \
             perform portable, POSIX-compliant checks"
        )
    )]
    portable: bool,

    #[arg(
        short = 'P',
        group = "mode",
        help = gettext(
            "Instead of performing checks based on the underlying file system, \
             check each component in pathname for basic validity"
        )
    )]
    basic: bool,

    #[arg(help = gettext("The pathnames to be checked"))]
    pathnames: Vec<String>,
}

fn check_path_basic(pathname: &str) -> Result<(), &'static str> {
    if pathname.is_empty() {
        return Err("empty pathname");
    }

    for component in Path::new(pathname).components() {
        if let Component::Normal(filename) = component {
            if filename.to_string_lossy().starts_with("-") {
                return Err("filename begins with -");
            }
        }
    }

    Ok(())
}

fn check_path_limits(
    pathname: &str,
    max_path: usize,
    max_name: usize,
    check_ascii: bool,
) -> Result<(), &'static str> {
    if pathname.len() > max_path {
        return Err("pathname too long");
    }

    for component in Path::new(pathname).components() {
        if let Component::Normal(filename) = component {
            if filename.len() > max_name {
                return Err("filename too long");
            }
            if check_ascii && !filename.is_ascii() {
                return Err("filename contains non-portable characters");
            }
        }
    }

    Ok(())
}

// find the first existing directory in the path
fn find_fshandle(pathname: &str) -> Result<String, &'static str> {
    let mut path = Path::new(pathname);
    let mut fsh = String::new();

    while !path.exists() {
        match path.parent() {
            Some(parent) => {
                fsh = parent.to_string_lossy().to_string();
                path = parent;
            }
            None => {
                fsh = path.to_string_lossy().to_string();
                break;
            }
        }
    }

    Ok(fsh)
}

fn check_path_posix(pathname: &str) -> Result<(), &'static str> {
    check_path_limits(pathname, _POSIX_PATH_MAX, _POSIX_NAME_MAX, true)
}

fn check_path_fs(pathname: &str) -> Result<(), &'static str> {
    let fsh = find_fshandle(pathname)?;
    let fsh = CString::new(fsh).unwrap();

    let path_max = unsafe { libc::pathconf(fsh.as_ptr(), libc::_PC_PATH_MAX) };
    if path_max < 0 {
        return Err("pathconf error(path length)");
    }
    let name_max = unsafe { libc::pathconf(fsh.as_ptr(), libc::_PC_NAME_MAX) };
    if name_max < 0 {
        return Err("pathconf error(name length)");
    }

    check_path_limits(pathname, path_max as usize, name_max as usize, false)
}

fn check_path(args: &Args, pathname: &str) -> Result<(), &'static str> {
    if args.portable {
        check_path_posix(pathname)
    } else if args.basic {
        check_path_basic(pathname)
    } else {
        check_path_fs(pathname)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    for pathname in &args.pathnames {
        if let Err(e) = check_path(&args, pathname) {
            exit_code = 1;
            eprintln!("{}: {}", pathname, e);
        }
    }

    std::process::exit(exit_code)
}
