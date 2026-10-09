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
use std::{thread, time::Duration};

#[derive(Parser)]
#[command(version, about = gettext("sleep - suspend execution for an interval"))]
struct Args {
    #[arg(
        value_parser = parse_interval,
        help = gettext("Number of seconds to sleep")
    )]
    seconds: Duration,
}

/// The `time` operand: POSIX's non-negative decimal integer (0 included), or, as GNU and BSD
/// sleep accept, one with a fraction: `digits.digits`, `.digits` or `digits.`. A fraction finer
/// than a nanosecond rounds up, so the sleep is never shorter than asked. Nothing else -- no
/// sign, exponent, unit suffix or blank -- is a number here.
fn parse_interval(operand: &str) -> Result<Duration, String> {
    let invalid = || gettext!("invalid time interval '{}'", operand);
    let (whole, fraction) = operand.split_once('.').unwrap_or((operand, ""));
    let all_digits = |s: &str| s.bytes().all(|b| b.is_ascii_digit());
    if whole.len() + fraction.len() == 0 || !all_digits(whole) || !all_digits(fraction) {
        return Err(invalid());
    }
    let secs: u64 = if whole.is_empty() {
        0
    } else {
        whole.parse().map_err(|_| invalid())?
    };
    let mut nanos: u32 = 0;
    for digit in fraction.bytes().chain(std::iter::repeat(b'0')).take(9) {
        nanos = nanos * 10 + u32::from(digit - b'0');
    }
    let finer = fraction.bytes().skip(9).any(|b| b != b'0');
    let interval = Duration::new(secs, nanos);
    Ok(if finer {
        interval.saturating_add(Duration::from_nanos(1))
    } else {
        interval
    })
}

fn main() {
    plib::diag::init_locale("sleep");

    let args = plib::optarg::parse::<Args>();

    // Ignore the SIGALRM signal (Windows has none).
    #[cfg(unix)]
    unsafe {
        libc::signal(libc::SIGALRM, libc::SIG_IGN);
    }

    thread::sleep(args.seconds);
}
