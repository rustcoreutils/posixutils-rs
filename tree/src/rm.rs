//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate atty;
extern crate clap;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, textdomain};
use plib::PROJECT_NAME;
use std::ffi::OsStr;
use std::fs;
use std::io::{self, BufRead, Write};
use walkdir::WalkDir;

/// rm - remove directory entries
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Do not prompt for confirmation.
    #[arg(short, long)]
    force: bool,

    /// Prompt for confirmation.
    #[arg(short, long)]
    interactive: bool,

    /// Remove file hierarchies.
    #[arg(short, short_alias = 'R', long)]
    recurse: bool,

    /// Filepaths to remove
    files: Vec<String>,
}

struct RmConfig {
    args: Args,
    is_tty: bool,
}

fn prompt(cfg: &RmConfig, filepath: &OsStr, metadata: &fs::Metadata) -> bool {
    let writable = !metadata.permissions().readonly();

    let do_prompt = cfg.args.interactive || (!cfg.args.force && !writable && cfg.is_tty);
    if !do_prompt {
        return true;
    }

    let prompt = format!(
        "{} {}? ",
        gettext("remove read-only file"),
        filepath.to_string_lossy()
    );
    io::stdout()
        .write_all(prompt.as_bytes())
        .expect("stdout failure");
    io::stdout().flush().expect("stdout failure");

    let mut line = String::new();
    io::stdin()
        .lock()
        .read_line(&mut line)
        .expect("stdin read failure");

    line.starts_with("y") || line.starts_with("Y")
}

fn rm_file(cfg: &RmConfig, filepath: &OsStr, metadata: &fs::Metadata) -> io::Result<()> {
    if prompt(cfg, filepath, metadata) {
        fs::remove_file(filepath)
    } else {
        Ok(())
    }
}

fn rm_dir_simple(cfg: &RmConfig, filepath: &OsStr, metadata: &fs::Metadata) -> io::Result<()> {
    if prompt(cfg, filepath, metadata) {
        fs::remove_dir(filepath)
    } else {
        Ok(())
    }
}

fn rm_directory(cfg: &RmConfig, filepath: &OsStr) -> io::Result<()> {
    if !cfg.args.recurse {
        eprintln!(
            "{} {}",
            filepath.to_string_lossy(),
            gettext("is a directory; not following")
        );
        return Ok(());
    }

    for entry in WalkDir::new(filepath)
        .contents_first(true)
        .follow_links(false)
    {
        let entry = entry?;
        let subname = entry.path().as_os_str();
        let sub_metadata = entry.metadata()?;
        if sub_metadata.is_dir() {
            rm_dir_simple(cfg, subname, &sub_metadata)?;
        } else {
            rm_file(cfg, subname, &sub_metadata)?;
        }
    }

    Ok(())
}

fn rm_path(cfg: &RmConfig, filepath: &OsStr) -> io::Result<()> {
    let metadata = fs::metadata(filepath)?;

    if metadata.is_dir() {
        rm_directory(cfg, filepath)
    } else {
        rm_file(cfg, filepath, &metadata)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let is_tty = atty::is(atty::Stream::Stdin);
    let cfg = RmConfig { args, is_tty };

    let mut exit_code = 0;

    for filepath_str in &cfg.args.files {
        let filepath = OsStr::new(filepath_str);
        if let Err(e) = rm_path(&cfg, &filepath) {
            exit_code = 1;
            if !cfg.args.force {
                eprintln!("{}: {}", filepath.to_string_lossy(), e);
            }
        }
    }

    std::process::exit(exit_code)
}
