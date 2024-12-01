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
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    unsafe {
        // Ignore the SIGALRM signal
        libc::signal(libc::SIGALRM, libc::SIG_IGN);
    }

    thread::sleep(time::Duration::from_secs(args.seconds));

    Ok(())
}
