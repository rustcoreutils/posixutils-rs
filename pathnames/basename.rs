//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::path::Path;

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};

#[derive(Parser)]
#[command(
    version,
    about = gettext("basename - return non-directory portion of a pathname")
)]
struct Args {
    pathname: String,

    suffix: Option<String>,
}

fn show_basename(args: &Args) {
    let mut pathname = args.pathname.clone();

    match &args.suffix {
        None => {}
        Some(suffix) => match pathname.strip_suffix(suffix) {
            None => {}
            Some(s) => {
                pathname = String::from(s);
            }
        },
    }

    if pathname.is_empty() || pathname == "." {
        println!("{}", pathname);
        return;
    }

    let path = Path::new(&pathname);

    println!(
        "{}",
        path.file_name()
            .expect("Input is not a pathname.")
            .to_string_lossy()
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    show_basename(&args);

    Ok(())
}
