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
use std::env;
use std::fs;
use std::fs::File;
use std::io::{ErrorKind, Read, Result, Write};
use std::process::{exit, ExitStatus};

#[derive(Parser)]
struct CronArgs {
    #[arg(short, long, help = gettext("edit user's crontab"))]
    edit: bool,
    #[arg(short, long, help = gettext("list user's crontab"))]
    list: bool,
    #[arg(short, long, help = gettext("delete user's crontab"))]
    remove: bool,
    #[arg(name = "FILE", help = gettext("file to replace user's current crontab with"))]
    file: Option<String>,
}

fn print_usage(err: &str) {
    let name = env::args().next().unwrap();
    eprintln!("{name}: usage error: {err}");
    eprintln!("usage:\t{name} [ FILE ]");
    eprintln!("\t{name} [ -e | -l | -r ]");
    eprintln!("\t-e\t- edit user's crontab");
    eprintln!("\t-l\t- list user's crontab");
    eprintln!("\t-r\t- delete user's crontab");
}

fn list_crontab(path: &str) -> Result<String> {
    fs::read_to_string(path)
}

fn remove_crontab(path: &str) -> Result<()> {
    fs::remove_file(path)
}

fn edit_crontab(path: &str) -> Result<ExitStatus> {
    File::create(path)?;
    let editor = env::var("EDITOR").unwrap_or("vi".to_string());
    let shell = env::var("SHELL").unwrap_or("sh".to_string());
    let args = ["-c".to_string(), format!("{editor} {path}")];
    std::process::Command::new(shell).args(args).status()
}

fn replace_crontab(from: &str, to: &str) -> Result<()> {
    let mut source = File::open(from)?;
    let mut target = File::create(to)?;
    let mut buffer = Vec::new();

    source.read_to_end(&mut buffer)?;
    target.write_all(&buffer)?;

    Ok(())
}

fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = CronArgs::parse();
    let Ok(logname) = env::var("LOGNAME") else {
        println!("Could not obtain the user's logname.");
        exit(1);
    };
    #[cfg(target_os = "linux")]
    let path = format!("/var/spool/cron/{logname}");
    #[cfg(target_os = "macos")]
    let path = format!("/var/spool/cron/{logname}");

    let opt_count = [args.edit, args.list, args.remove, args.file.is_some()]
        .into_iter()
        .map(|x| x as i32)
        .sum::<i32>();
    if opt_count > 1 {
        print_usage("Too many options specified.");
        exit(1);
    }

    if opt_count < 1 {
        print_usage("Not enough options specified.");
        exit(1);
    }

    if args.edit {
        match edit_crontab(&path) {
            Ok(status) => exit(status.code().unwrap_or(0)),
            Err(err) => {
                match err.kind() {
                    ErrorKind::NotFound => println!("No crontab file has been found."),
                    ErrorKind::PermissionDenied => {
                        println!("Permission to access user's crontab file denied.")
                    }
                    ErrorKind::Interrupted => println!("crontab was interrupted."),
                    ErrorKind::OutOfMemory => println!("crontab exceeded available memory."),
                    _ => println!("Unknown error: {}", err),
                }
                exit(1);
            }
        }
    }

    if args.list {
        match list_crontab(&path) {
            Ok(content) => println!("{}", content),
            Err(err) => {
                match err.kind() {
                    ErrorKind::NotFound => println!("No crontab file has been found."),
                    ErrorKind::PermissionDenied => {
                        println!("Permission to access user's crontab file denied.")
                    }
                    ErrorKind::Interrupted => println!("crontab was interrupted."),
                    ErrorKind::OutOfMemory => println!("crontab exceeded available memory."),
                    _ => println!("Unknown error: {}", err),
                }
                exit(1);
            }
        }
    }

    if args.remove {
        match remove_crontab(&path) {
            Ok(()) => println!("Removed crontab file"),
            Err(err) => {
                match err.kind() {
                    ErrorKind::NotFound => println!("No crontab file has been found."),
                    ErrorKind::PermissionDenied => {
                        println!("Permission to access user's crontab file denied.")
                    }
                    ErrorKind::Interrupted => println!("crontab was interrupted."),
                    ErrorKind::OutOfMemory => println!("crontab exceeded available memory."),
                    _ => println!("Unknown error: {}", err),
                }
                exit(1);
            }
        }
    }

    if let Some(file) = args.file {
        match replace_crontab(&file, &path) {
            Ok(()) => println!("Replaced crontab file with {file}"),
            Err(err) => {
                match err.kind() {
                    ErrorKind::NotFound => {
                        println!("Crontab file or user-specified file has not been found.")
                    }
                    ErrorKind::PermissionDenied => println!(
                        "Permission to access user's crontab file or user-specified file denied."
                    ),
                    ErrorKind::Interrupted => println!("crontab was interrupted."),
                    ErrorKind::OutOfMemory => println!("crontab exceeded available memory."),
                    _ => println!("Unknown error: {}", err),
                }
                exit(1);
            }
        }
    }

    Ok(())
}
