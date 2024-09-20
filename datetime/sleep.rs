//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::{thread, time};

/// sleep - suspend execution for an interval
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Number of seconds to sleep
    #[arg(value_parser = clap::value_parser!(u64).range(1..))]
    seconds: u64,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    thread::sleep(time::Duration::from_secs(args.seconds));

    Ok(())
}
