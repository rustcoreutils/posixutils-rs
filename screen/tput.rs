//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - eliminate unwrap. more error checking.
// - read init-file and reset-file data from filesystem
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::io;
use terminfo::{capability as cap, Database};

/// tput - change terminal characteristics
#[derive(Parser)]
#[command(author, version, about, long_about)]
struct Args {
    /// Indicate the type of terminal.
    #[arg(short = 'T', long)]
    term: Option<String>,

    /// Terminal operand to execute
    operand: String,
}

fn tput_init(info: Database) -> terminfo::Result<()> {
    if let Some(cap) = info.get::<cap::Init1String>() {
        cap.expand().to(io::stdout())?;
    }
    if let Some(cap) = info.get::<cap::Init2String>() {
        cap.expand().to(io::stdout())?;
    }
    if let Some(cap) = info.get::<cap::InitFile>() {
        // FIXME
        cap.expand().to(io::stdout())?;
    }
    if let Some(cap) = info.get::<cap::Init3String>() {
        cap.expand().to(io::stdout())?;
    }

    Ok(())
}

fn tput_reset(info: Database) -> terminfo::Result<()> {
    if let Some(cap) = info.get::<cap::Reset1String>() {
        cap.expand().to(io::stdout())?;
    }
    if let Some(cap) = info.get::<cap::Reset2String>() {
        cap.expand().to(io::stdout())?;
    }
    if let Some(cap) = info.get::<cap::ResetFile>() {
        // FIXME
        cap.expand().to(io::stdout())?;
    }
    if let Some(cap) = info.get::<cap::Reset3String>() {
        cap.expand().to(io::stdout())?;
    }

    Ok(())
}

fn tput_clear(info: Database) -> terminfo::Result<()> {
    if let Some(clear) = info.get::<cap::ClearScreen>() {
        clear.expand().to(io::stdout())?;
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let info = match args.term {
        None => Database::from_env().unwrap(),
        Some(termtype) => Database::from_name(termtype).unwrap(),
    };

    match args.operand.as_str() {
        "clear" => tput_clear(info)?,
        "init" => tput_init(info)?,
        "reset" => tput_reset(info)?,
        _ => {
            eprintln!("{}", gettext("Unknown terminal command"));
            std::process::exit(1);
        }
    }

    Ok(())
}
