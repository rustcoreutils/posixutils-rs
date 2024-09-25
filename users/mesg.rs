//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - use .metadata() and std::os::unix::fs::PermissionsExt if possible
// - set process exit code according to spec
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::io::{self, Error, ErrorKind, IsTerminal};
use std::mem;

/// mesg - permit or deny messages
#[derive(Parser)]
#[command(author, version, about, long_about)]
struct Args {
    /// "y" or "n":  Grant (y) or deny (n) permission to other users to send messages to the terminal device.
    operand: Option<String>,
}

enum Stream {
    Stdin,
    Stdout,
    Stderr,
}

fn find_tty() -> Option<Stream> {
    if io::stdin().is_terminal() {
        Some(Stream::Stdin)
    } else if io::stdout().is_terminal() {
        Some(Stream::Stdout)
    } else if io::stderr().is_terminal() {
        Some(Stream::Stderr)
    } else {
        None
    }
}

fn tty_to_fd(tty: Stream) -> i32 {
    match tty {
        Stream::Stdin => libc::STDIN_FILENO,
        Stream::Stdout => libc::STDOUT_FILENO,
        Stream::Stderr => libc::STDERR_FILENO,
    }
}

fn stat_tty() -> io::Result<(i32, libc::stat)> {
    let tty_res = find_tty();
    if tty_res.is_none() {
        return Err(Error::new(ErrorKind::Other, "tty not found"));
    }
    let fd = tty_to_fd(tty_res.unwrap());

    unsafe {
        let mut st: libc::stat = mem::zeroed();
        let ret = libc::fstat(fd, &mut st);
        if ret < 0 {
            eprintln!("{}", gettext("fstat failed"));
            return Err(io::Error::last_os_error());
        }

        Ok((fd, st))
    }
}

fn show_mesg(st: libc::stat) -> io::Result<()> {
    if (st.st_mode & (libc::S_IWGRP | libc::S_IWOTH)) != 0 {
        println!("is y");
    } else {
        println!("is n");
    }
    Ok(())
}

fn parse_setting(setting: &str) -> Result<bool, &'static str> {
    match setting {
        "y" | "Y" => Ok(true),
        "n" | "N" => Ok(false),
        _ => Err("invalid operand"),
    }
}

fn set_mesg(fd: i32, st: libc::stat, setting: &str) -> io::Result<()> {
    let res = parse_setting(setting);
    if let Err(e) = res {
        return Err(Error::new(ErrorKind::Other, e));
    }
    let affirm = res.unwrap();

    let mut mode = st.st_mode;

    if affirm {
        if (mode & (libc::S_IWGRP | libc::S_IWOTH)) != 0 {
            return Ok(());
        }

        mode = mode | libc::S_IWGRP | libc::S_IWOTH;
    } else {
        if (mode & (libc::S_IWGRP | libc::S_IWOTH)) == 0 {
            return Ok(());
        }

        mode = mode & !(libc::S_IWGRP | libc::S_IWOTH);
    }

    let chres = unsafe { libc::fchmod(fd, mode) };
    if chres < 0 {
        eprintln!("{}", gettext("failed to change terminal mode"));
        return Err(io::Error::last_os_error());
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let (fd, stat) = stat_tty()?;

    match args.operand {
        None => show_mesg(stat)?,
        Some(op) => set_mesg(fd, stat, &op)?,
    }

    Ok(())
}
