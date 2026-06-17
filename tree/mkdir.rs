//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use modestr::ChmodMode;
use plib::modestr;
use std::ffi::CString;
use std::io;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

/// mkdir - make directories
#[derive(Parser)]
#[command(version, about = gettext("mkdir - make directories"))]
struct Args {
    #[arg(short, long, help = gettext("Create any missing intermediate pathname components"))]
    parents: bool,

    #[arg(short, long, help = gettext("Set the file permission bits of the newly-created directory to the specified mode value"))]
    mode: Option<String>,

    #[arg(help = gettext("A pathname of a directory to be created"))]
    dirs: Vec<String>,
}

fn get_umask() -> libc::mode_t {
    // No portable way to read the umask without temporarily setting it.
    unsafe {
        let u = libc::umask(0);
        libc::umask(u);
        u
    }
}

/// Create the directory `path` with the given mode. When `bypass_umask` is set, the umask is
/// temporarily cleared so the directory ends up with exactly `mode` (used for an explicit `-m` and
/// for `-p` intermediate components). Returns `Ok(false)` if the path already exists as a
/// directory (so `-p` can skip it), `Ok(true)` if newly created.
fn make_dir(path: &Path, mode: libc::mode_t, bypass_umask: bool) -> io::Result<bool> {
    let bytes = path.as_os_str().as_bytes();
    if bytes.contains(&b'\n') {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            gettext("pathname contains a <newline> character"),
        ));
    }
    let c_path = CString::new(bytes).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    let saved = if bypass_umask {
        Some(unsafe { libc::umask(0) })
    } else {
        None
    };
    let ret = unsafe { libc::mkdir(c_path.as_ptr(), mode) };
    let err = io::Error::last_os_error();
    if let Some(prev) = saved {
        unsafe { libc::umask(prev) };
    }

    if ret == 0 {
        Ok(true)
    } else if err.raw_os_error() == Some(libc::EEXIST) && path.is_dir() {
        Ok(false)
    } else {
        Err(err)
    }
}

fn do_mkdir(dirname: &str, mode: &ChmodMode, parents: bool, explicit_mode: bool) -> io::Result<()> {
    // Cast for macOS, where libc mode constants are u16.
    #[allow(clippy::unnecessary_cast)]
    let leaf_mode = (match mode {
        ChmodMode::Absolute(mode, _) => *mode,
        ChmodMode::Symbolic(sym) => modestr::mutate(0o777, true, sym),
    }) as libc::mode_t;

    if parents {
        // POSIX: intermediate components get the default mode modified by umask, plus write and
        // search permission for the owner, so the descendants can always be created.
        #[allow(clippy::unnecessary_cast)]
        let inter_mode = ((0o777 & !(get_umask() as u32)) | 0o300) as libc::mode_t;

        let parts: Vec<&str> = dirname.split('/').filter(|p| !p.is_empty()).collect();
        let last = parts.len().saturating_sub(1);
        let mut path = if dirname.starts_with('/') {
            PathBuf::from("/")
        } else {
            PathBuf::new()
        };
        for (i, part) in parts.iter().enumerate() {
            path.push(part);
            if i == last {
                make_dir(&path, leaf_mode, explicit_mode)?;
            } else {
                make_dir(&path, inter_mode, true)?;
            }
        }
        Ok(())
    } else if make_dir(Path::new(dirname), leaf_mode, explicit_mode)? {
        Ok(())
    } else {
        // Without `-p`, an existing directory is an error.
        Err(io::Error::from_raw_os_error(libc::EEXIST))
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    // parse the mode string
    let explicit_mode = args.mode.is_some();
    let mode = match args.mode {
        Some(mode) => modestr::parse(&mode)?,
        None => ChmodMode::Absolute(0o777, 3),
    };

    // apply the mode to each file
    for dirname in &args.dirs {
        if let Err(e) = do_mkdir(dirname, &mode, args.parents, explicit_mode) {
            exit_code = 1;
            eprintln!("mkdir: {}: {}", dirname, e);
        }
    }

    std::process::exit(exit_code)
}
