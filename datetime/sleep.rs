//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use libc::{signal, SIGALRM, SIG_IGN};
use plib::PROJECT_NAME;
use std::{thread, time};

#[derive(Parser)]
#[command(version, about = gettext("sleep - suspend execution for an interval"))]
struct Args {
    #[arg(
        value_parser = clap::value_parser!(u64).range(1..),
        help = gettext("Number of seconds to sleep")
    )]
    seconds: u64,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    unsafe {
        // Ignore the SIGALRM signal
        signal(SIGALRM, SIG_IGN);
    }

    thread::sleep(time::Duration::from_secs(args.seconds));

    Ok(())
}
