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
use plib::PROJECT_NAME;
use std::ffi::OsStr;
use std::io::{self, IsTerminal, Write};
use std::path::PathBuf;
use std::process::{Command, Output, Stdio};
use thiserror::Error;

// `/usr/share/man` - system provided directory with system documentation.
// `/usr/local/share/man` - user programs provided directory with system documentation.
const MAN_PATHS: [&str; 2] = ["/usr/share/man", "/usr/local/share/man"];
// Prioritized order of sections.
const MAN_SECTIONS: [i8; 9] = [1, 8, 2, 3, 4, 5, 6, 7, 9];

#[derive(Parser)]
#[command(version, about = gettext("man - display system documentation"))]
struct Args {
    #[arg(short, help = gettext("Interpret name operands as keywords for searching the summary database."))]
    keyword: bool,

    #[arg(help = gettext("Names of the utilities or keywords to display documentation for."))]
    names: Vec<String>,
}

#[derive(Error, Debug)]
enum ManError {
    #[error("man paths to man pages doesn't exist")]
    ManPaths,
    #[error("no names specified")]
    NoNames,
    #[error("system documentation for \"{0}\" not found")]
    PageNotFound(String),
    #[error("failed to get terminal size")]
    GetTerminalSize,
    #[error("neither groff(1), nor nroff(1), nor mandoc(1) are installed")]
    NoFormatters,
    #[error("{0} command not found")]
    CommandNotFound(String),
    #[error("failed to execute command: {0}")]
    Io(#[from] io::Error),
}

/// Gets system documentation path by passed name.
///
/// # Arguments
///
/// `name` - [str] name of necessary system documentation.
///
/// # Returns
///
/// [PathBuf] of found system documentation.
///
/// # Errors
///
/// [ManError] if file not found.
fn get_man_page_path(name: &str) -> Result<PathBuf, ManError> {
    MAN_PATHS
        .iter()
        .flat_map(|path| {
            MAN_SECTIONS.iter().flat_map(move |section| {
                let base_path = format!("{path}/man{section}/{name}.{section}");
                vec![format!("{base_path}.gz"), base_path]
            })
        })
        .find(|path| PathBuf::from(path).exists())
        .map(PathBuf::from)
        .ok_or_else(|| ManError::PageNotFound(name.to_string()))
}

/// Spawns process with arguments and STDIN if present.
///
/// # Arguments
///
/// `name` - [str] name of process.
/// `args` - [IntoIterator<Item = AsRef<OsStr>>] arguments of process.
/// `stdin` - [Option<&[u8]>] STDIN content of process.
///
/// # Returns
///
/// [Output] of spawned process.
///
/// # Errors
///
/// [ManError] if process spawn failed or failed to get its output.
fn spawn<I, S>(name: &str, args: I, stdin: Option<&[u8]>, stdout: Stdio) -> Result<Output, ManError>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let mut process = Command::new(name)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(stdout)
        .spawn()
        .map_err(|err| match err.kind() {
            io::ErrorKind::NotFound => ManError::CommandNotFound(name.to_string()),
            _ => ManError::Io(err),
        })?;

    if let Some(stdin) = stdin {
        if let Some(mut process_stdin) = process.stdin.take() {
            process_stdin.write_all(stdin)?;
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                format!("failed to open stdin for {name}"),
            ))?;
        }
    }

    let output = process.wait_with_output().map_err(|_| {
        io::Error::new(io::ErrorKind::Other, format!("failed to get {name} stdout"))
    })?;

    if !output.status.success() {
        Err(io::Error::new(
            io::ErrorKind::Other,
            format!("{name} failed"),
        ))?
    } else {
        Ok(output)
    }
}

/// Gets system documentation content by passed name.
///
/// # Arguments
///
/// `name` - [str] name of necessary system documentation.
///
/// # Returns
///
/// [Vec<u8>] output of called `*cat` command.
///
/// # Errors
///
/// [ManError] if file not found or failed to execute `*cat` command.
fn get_man_page(name: &str) -> Result<Vec<u8>, ManError> {
    let man_page_path = get_man_page_path(name)?;

    let cat_process_name = if man_page_path.extension().and_then(|ext| ext.to_str()) == Some("gz") {
        "zcat"
    } else {
        "cat"
    };

    let output = spawn(cat_process_name, &[man_page_path], None, Stdio::piped())?;
    Ok(output.stdout)
}

/// Gets page width.
///
/// # Returns
///
/// [Option<u16>] width value of current terminal. [Option::Some] if working on terminal and receiving terminal size was succesfull. [Option::None] if working not on terminal.
///
/// # Errors
///
/// Returns [ManError] if working on terminal and failed to get terminal size.
fn get_page_width() -> Result<Option<u16>, ManError> {
    if !std::io::stdout().is_terminal() {
        return Ok(None);
    }
    let mut winsize = libc::winsize {
        ws_row: 0,
        ws_col: 0,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };
    let result = unsafe { libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &mut winsize) };
    if result != 0 {
        return Err(ManError::GetTerminalSize);
    }
    let result_width = if winsize.ws_col >= 80 {
        winsize.ws_col - 2
    } else {
        winsize.ws_col
    };
    Ok(Some(result_width))
}

/// Gets formated by `groff(1)` system documentation.
///
/// # Arguments
///
/// `man_page` - [&[u8]] with content that needs to be formatted.
/// `width` - [Option<u16>] width value of current terminal.
///
/// # Returns
///
/// [Vec<u8>] STDOUT of called `groff(1)` formatter.
///
/// # Errors
///
/// [ManError] if file failed to execute `groff(1)` formatter.
fn groff_format(man_page: &[u8], width: Option<u16>) -> Result<Vec<u8>, ManError> {
    let mut args = vec![
        "-Tutf8",
        "-S",
        "-P-h",
        "-Wall",
        "-mtty-char",
        "-t",
        "-mandoc",
    ];
    let width = width.map(|w| (format!("-rLL={w}n"), format!("-rLR={w}n")));
    if let Some((rll, rlr)) = width.as_ref() {
        args.push(rll);
        args.push(rlr);
    }

    spawn("groff", &args, Some(man_page), Stdio::piped()).map(|output| output.stdout)
}

/// Gets formated by `nroff(1)` system documentation.
///
/// # Arguments
///
/// `man_page` - [&[u8]] with content that needs to be formatted.
/// `width` - [Option<u16>] width value of current terminal.
///
/// # Returns
///
/// [Vec<u8>] STDOUT of called `nroff(1)` formatter.
///
/// # Errors
///
/// [ManError] if file failed to execute `nroff(1)` formatter.
fn nroff_format(man_page: &[u8], width: Option<u16>) -> Result<Vec<u8>, ManError> {
    let mut args = vec!["-Tutf8", "-S", "-Wall", "-mtty-char", "-t", "-mandoc"];
    let width = width.map(|w| (format!("-rLL={w}n"), format!("-rLR={w}n")));
    if let Some((rll, rlr)) = width.as_ref() {
        args.push(rll);
        args.push(rlr);
    }

    spawn("nroff", &args, Some(man_page), Stdio::piped()).map(|output| output.stdout)
}

/// Gets formatted by `mandoc(1)` system documentation.
///
/// # Arguments
///
/// `man_page` - [&[u8]] with content that needs to be formatted.
/// `width` - [Option<u16>] width value of current terminal.
///
/// # Returns
///
/// [Vec<u8>] STDOUT of called `mandoc(1)` formatter.
///
/// # Errors
///
/// [ManError] if file failed to execute `mandoc(1)` formatter.
fn mandoc_format(man_page: &[u8], width: Option<u16>) -> Result<Vec<u8>, ManError> {
    let mut args = vec![];
    let width = width.map(|w| format!("width={w}"));
    if let Some(width) = width.as_ref() {
        args.push("-O");
        args.push(width);
    }

    spawn("mandoc", &args, Some(man_page), Stdio::piped()).map(|output| output.stdout)
}

/// Formats man page content into appropriate format.
///
/// # Arguments
///
/// `man_page` - [Vec<u8>] with content that needs to be formatted.
///
/// # Returns
///
/// [Vec<u8>] STDOUT of called formatter.
///
/// # Errors
///
/// [ManError] if failed to execute formatter.
fn format_man_page(man_page: Vec<u8>) -> Result<Vec<u8>, ManError> {
    let width = get_page_width()?;

    let formatters = [groff_format, nroff_format, mandoc_format];

    for formatter in &formatters {
        match formatter(&man_page, width) {
            Ok(formatted_man_page) => return Ok(formatted_man_page),
            Err(ManError::CommandNotFound(_)) => continue,
            Err(err) => return Err(err),
        }
    }

    Err(ManError::NoFormatters)
}

/// Formats man page content into appropriate format.
///
/// # Arguments
///
/// `man_page` - [Vec<u8>] with content that needs to displayed.
///
/// # Errors
///
/// [ManError] if failed to execute pager or failed write to its STDIN.
fn display_pager(man_page: Vec<u8>) -> Result<(), ManError> {
    let pager = std::env::var("PAGER").unwrap_or_else(|_| "more".to_string());

    let args = if pager.ends_with("more") {
        vec!["-s"]
    } else {
        vec![]
    };

    spawn(&pager, args, Some(&man_page), Stdio::inherit())?;

    Ok(())
}

/// Displays man page
///
/// # Arguments
///
/// `name` - [str] name of system documentation.
///
/// # Errors
///
/// [ManError] if man page not found, or any display error happened.
fn display_man_page(name: &str) -> Result<(), ManError> {
    let cat_output = get_man_page(name)?;
    let formatter_output = format_man_page(cat_output)?;
    display_pager(formatter_output)?;

    Ok(())
}

/// Displays man page summaries for the given keyword.
///
/// # Arguments
///
/// `keyword` - [str] name of keyword.
///
/// # Returns
///
/// [true] if `apropos` finished successfully, otherwise [false].
///
/// # Errors
///
/// [ManError] if call of `apropros` utility failed.
fn display_summary_database(keyword: &str) -> Result<bool, ManError> {
    let exit_status = Command::new("apropos").arg(keyword).spawn()?.wait()?;

    if exit_status.success() {
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Main function that handles the program logic. It processes the input
/// arguments, and either displays man pages or searches the summary database.
///
/// # Arguments
///
/// `args` - [Args] set of incoming arguments.
///
/// # Returns
///
/// [true] if no non-critical error happend, otherwise [false].
///
/// # Errors
///
/// [ManError] if critical error happened.
fn man(args: Args) -> Result<bool, ManError> {
    let any_path_exists = MAN_PATHS.iter().any(|path| PathBuf::from(path).exists());

    if !any_path_exists {
        return Err(ManError::ManPaths);
    }

    if args.names.is_empty() {
        return Err(ManError::NoNames);
    }

    let mut no_errors = true;
    if args.keyword {
        for name in &args.names {
            if !display_summary_database(name)? {
                no_errors = false;
            }
        }
    } else {
        for name in &args.names {
            if let Err(err) = display_man_page(name) {
                no_errors = false;
                eprintln!("man: {err}");
            }
        }
    };

    Ok(no_errors)
}

// Exit code:
//     0 - Successful completion.
//     >0 - An error occurred.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    // parse command line arguments
    let args = Args::parse();

    let exit_code = match man(args) {
        Ok(true) => 0,
        // Some error for specific `name`
        Ok(false) => 1,
        // Any critical error happened
        Err(err) => {
            eprintln!("man: {err}");
            1
        }
    };

    std::process::exit(exit_code)
}
