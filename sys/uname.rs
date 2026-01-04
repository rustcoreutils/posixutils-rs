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

/// uname - return system name
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

/// Number of uname info fields: sysname, nodename, release, version, machine
const UNAME_FIELD_COUNT: usize = 5;

fn print_info(args: &Args, info: uname::Info) {
    let mut outs = Vec::with_capacity(UNAME_FIELD_COUNT);

    if args.system {
        outs.push(info.sysname);
    }
    if args.node {
        outs.push(info.nodename);
    }
    if args.release {
        outs.push(info.release);
    }
    if args.osversion {
        outs.push(info.version);
    }
    if args.machine {
        outs.push(info.machine);
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

    match uname::uname() {
        Ok(info) => print_info(&args, info),
        Err(e) => {
            eprintln!("{}", e);
            exit_code = 1;
        }
    }

    std::process::exit(exit_code)
}
