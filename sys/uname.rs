//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;

/// uname - return system name
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Behave as though all of the options -mnrsv were specified.
    #[arg(short, long)]
    all: bool,

    /// Write the name of the hardware type on which the system is running to standard output.
    #[arg(short, long)]
    machine: bool,

    /// Write the name of this node within an implementation-defined communications network.
    #[arg(short, long)]
    node: bool,

    /// Write the current release level of the operating system implementation.
    #[arg(short, long)]
    release: bool,

    /// Write the name of the implementation of the operating system.
    #[arg(short, long)]
    system: bool,

    /// Write the current version level of this release of the operating system implementation.
    #[arg(short = 'v', long)]
    osversion: bool,
}

fn print_info(args: &Args, info: uname::Info) {
    let mut outs = Vec::new();

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
    // parse command line arguments
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

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

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
