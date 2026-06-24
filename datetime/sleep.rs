//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use std::{thread, time};

#[derive(Parser)]
#[command(version, about = gettext("sleep - suspend execution for an interval"))]
struct Args {
    #[arg(
        // POSIX: the `time` operand is a non-negative decimal integer, so 0 is valid.
        value_parser = clap::value_parser!(u64).range(0..),
        help = gettext("Number of seconds to sleep")
    )]
    seconds: u64,
}

fn main() {
    plib::diag::init_locale("sleep");

    let args = Args::parse();

    unsafe {
        // Ignore the SIGALRM signal
        libc::signal(libc::SIGALRM, libc::SIG_IGN);
    }

    thread::sleep(time::Duration::from_secs(args.seconds));
}
