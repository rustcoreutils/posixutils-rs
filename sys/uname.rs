//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::CStr;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

/// uname - return system name
//
// The long-option aliases (--all, --machine, …, --osversion) are non-POSIX
// extensions retained for convenience; POSIX defines only the single-letter
// options. `-v`'s long form is `--osversion` because clap reserves `--version`.
#[derive(Parser)]
#[command(version, about = gettext("uname - return system name"))]
struct Args {
    #[arg(short, long, help = gettext("Behave as though all of the options -mnrsv were specified"))]
    all: bool,

    #[arg(short, long, help = gettext("Write the name of the hardware type on which the system is running to standard output"))]
    machine: bool,

    #[arg(short, long, help = gettext("Write the name of this node within an implementation-defined communications network"))]
    node: bool,

    #[arg(short, long, help = gettext("Write the current release level of the operating system implementation"))]
    release: bool,

    #[arg(short, long, help = gettext("Write the name of the implementation of the operating system"))]
    system: bool,

    #[arg(short = 'v', long, help = gettext("Write the current version level of this release of the operating system implementation"))]
    osversion: bool,
}

/// The six POSIX `uname` fields obtained from `uname(2)`'s `utsname`.
struct UnameInfo {
    sysname: String,
    nodename: String,
    release: String,
    version: String,
    machine: String,
}

/// Convert a NUL-terminated `utsname` C-string field to an owned String.
fn field_to_string(field: &[libc::c_char]) -> String {
    // SAFETY: the kernel NUL-terminates each utsname field.
    unsafe { CStr::from_ptr(field.as_ptr()) }
        .to_string_lossy()
        .into_owned()
}

/// Query the system via `uname(2)` directly (no external crate).
fn get_uname() -> std::io::Result<UnameInfo> {
    let mut uts: libc::utsname = unsafe { std::mem::zeroed() };
    let rc = unsafe { libc::uname(&mut uts) };
    if rc != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(UnameInfo {
        sysname: field_to_string(&uts.sysname),
        nodename: field_to_string(&uts.nodename),
        release: field_to_string(&uts.release),
        version: field_to_string(&uts.version),
        machine: field_to_string(&uts.machine),
    })
}

/// Number of uname info fields: sysname, nodename, release, version, machine
const UNAME_FIELD_COUNT: usize = 5;

fn print_info(args: &Args, info: &UnameInfo) {
    let mut outs = Vec::with_capacity(UNAME_FIELD_COUNT);

    if args.system {
        outs.push(info.sysname.as_str());
    }
    if args.node {
        outs.push(info.nodename.as_str());
    }
    if args.release {
        outs.push(info.release.as_str());
    }
    if args.osversion {
        outs.push(info.version.as_str());
    }
    if args.machine {
        outs.push(info.machine.as_str());
    }

    println!("{}", outs.join(" "));
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    if args.all {
        args.machine = true;
        args.node = true;
        args.release = true;
        args.system = true;
        args.osversion = true;
    } else if !args.machine && !args.node && !args.release && !args.system && !args.osversion {
        args.system = true;
    }

    let mut exit_code = 0;

    match get_uname() {
        Ok(info) => print_info(&args, &info),
        Err(e) => {
            eprintln!("{}", e);
            exit_code = 1;
        }
    }

    std::process::exit(exit_code)
}
