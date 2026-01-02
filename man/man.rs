//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::{ArgAction, Parser, ValueEnum};
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use man_util::config::{parse_config_file, ManConfig};
use man_util::formatter::MdocFormatter;
use man_util::parser::MdocParser;
use std::ffi::OsStr;
use std::io::{self, IsTerminal, Write};
use std::num::ParseIntError;
use std::path::PathBuf;
use std::process::{Command, Output, Stdio};
use std::str::FromStr;
use std::string::FromUtf8Error;
use thiserror::Error;

mod man_util;

// `/usr/share/man` - system provided directory with system documentation.
// `/usr/local/share/man` - user programs provided directory with system documentation.
const MAN_PATHS: [&str; 3] = ["/usr/share/man", "/usr/X11R6/man", "/usr/local/share/man"];

// Prioritized order of sections.
const MAN_SECTIONS: [Section; 10] = [
    Section::S1,
    Section::S8,
    Section::S6,
    Section::S2,
    Section::S3,
    Section::S5,
    Section::S7,
    Section::S4,
    Section::S9,
    Section::S3p,
];

/// Possible default config file paths to check if `-C` is not provided.
const MAN_CONFS: [&str; 3] = [
    "/etc/man.conf",
    "/etc/examples/man.conf",
    "/etc/manpath.config",
];

#[derive(Parser, Debug, Default)]
#[command(
    version,
    disable_help_flag = true,
    about = gettext("man - display system documentation")
)]
struct Args {
    #[arg(short, long, help = gettext("Display all matching manual pages"))]
    all: bool,

    #[arg(
        short = 'C',
        long,
        help = gettext("Use the specified file instead of the default configuration file")
    )]
    config_file: Option<PathBuf>,

    #[arg(short, long, help = gettext("Copy the manual page to the standard output"))]
    copy: bool,

    #[arg(short = 'f', long, help = gettext("A synonym for whatis(1)"))]
    whatis: bool,

    #[arg(
        short = 'h',
        long,
        help = gettext("Display only the SYNOPSIS lines of the requested manual pages")
    )]
    synopsis: bool,

    #[arg(
        short = 'k',
        long,
        help = gettext("Interpret name operands as keywords for searching the summary database")
    )]
    apropos: bool,

    #[arg(
        short = 'l',
        long = "local-file",
        help = gettext("Interpret PAGE argument(s) as local filename(s)"),
        num_args = 1..
    )]
    local_file: Option<Vec<PathBuf>>,

    #[arg(
        short = 'M',
        value_delimiter = ':',
        help = gettext("Override the list of directories to search for manual pages")
    )]
    override_paths: Vec<PathBuf>,

    #[arg(
        short = 'm',
        value_delimiter = ':',
        help = gettext("Augment the list of directories to search for manual pages")
    )]
    augment_paths: Vec<PathBuf>,

    #[arg(
        short = 'S',
        help = gettext("Only show pages for the specified machine(1) architecture")
    )]
    subsection: Option<String>,

    #[arg(
        short = 's',
        value_enum,
        help = gettext("Only select manuals from the specified section")
    )]
    section: Option<Section>,

    #[arg(
        short = 'w',
        help = gettext("List the pathnames of all matching manual pages instead of displaying any of them")
    )]
    list_pathnames: bool,

    #[arg(
        long = "help",
        action = ArgAction::Help,
        help = gettext("Print help information")
    )]
    help: Option<bool>,

    #[arg(
        help = gettext("Names of the utilities or keywords to display documentation for"),
        num_args = 0..
    )]
    names: Vec<String>,
}

/// Common errors that might occur.
#[derive(Error, Debug)]
enum ManError {
    /// Search path to man pages isn't exists
    #[error("man paths to man pages doesn't exist")]
    ManPaths,

    /// Commands for searching documentation isn't exists
    #[error("no names specified")]
    NoNames,

    /// Man can't find documentation for choosen command
    #[error("system documentation for \"{0}\" not found")]
    PageNotFound(String),

    /// Configuration file was not found
    #[error("configuration file was not found: {0}")]
    ConfigFileNotFound(String),

    /// Can't get terminal size
    #[error("failed to get terminal size")]
    GetTerminalSize,

    /// Man can't find choosen command
    #[error("{0} command not found")]
    CommandNotFound(String),

    /// Can't execute command; read/write file
    #[error("failed to execute command: {0}")]
    Io(#[from] io::Error),

    /// Mdoc error
    #[error("parsing error: {0}")]
    Mdoc(#[from] man_util::parser::MdocError),

    /// Parsing error
    #[error("parsing error: {0}")]
    ParseError(#[from] ParseError),

    /// Not found error
    #[error("file: {0} was not found")]
    NotFound(PathBuf),
}

/// Parsing error types
#[derive(Error, Debug)]
enum ParseError {
    #[error("{0}")]
    ParseIntError(#[from] ParseIntError),

    #[error("{0}")]
    FromUtf8Error(#[from] FromUtf8Error),
}

/// Manual type
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, ValueEnum)]
pub enum Section {
    /// General commands (tools and utilities)
    S1,
    /// System calls and error numbers
    S2,
    /// Library functions
    S3,
    /// perl(1) programmer's reference guide
    S3p,
    /// Device drivers
    S4,
    /// File formats
    S5,
    /// Games
    S6,
    /// Miscellaneous information
    S7,
    /// System maintenance and operation commands
    S8,
    /// Kernel internals
    S9,
}

impl FromStr for Section {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "1" => Ok(Section::S1),
            "2" => Ok(Section::S2),
            "3" => Ok(Section::S3),
            "3p" => Ok(Section::S3p),
            "4" => Ok(Section::S4),
            "5" => Ok(Section::S5),
            "6" => Ok(Section::S6),
            "7" => Ok(Section::S7),
            "8" => Ok(Section::S8),
            "9" => Ok(Section::S9),
            _ => Err(format!("Invalid section: {}", s)),
        }
    }
}

impl std::fmt::Display for Section {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Section::S1 => "1",
            Section::S2 => "2",
            Section::S3 => "3",
            Section::S3p => "3p",
            Section::S4 => "4",
            Section::S5 => "5",
            Section::S6 => "6",
            Section::S7 => "7",
            Section::S8 => "8",
            Section::S9 => "9",
        };
        write!(f, "{}", s)
    }
}

/// Basic formatting settings for manual pages (width, indentation)
#[derive(Debug, Clone, Copy)]
pub struct FormattingSettings {
    /// Terminal width
    pub width: usize,
    /// Lines indentation
    pub indent: usize,
}

impl Default for FormattingSettings {
    fn default() -> Self {
        Self {
            width: 78,
            indent: 6,
        }
    }
}

//
// ──────────────────────────────────────────────────────────────────────────────
//  HELPER FUNCTIONS
// ──────────────────────────────────────────────────────────────────────────────
//

/// Try to locate the configuration file:
/// - If `path` is Some, check if it exists; error if not.
/// - If `path` is None, try each of MAN_CONFS; return an error if none exist.
fn get_config_file_path(path: &Option<PathBuf>) -> Result<PathBuf, ManError> {
    if let Some(user_path) = path {
        if user_path.exists() {
            Ok(user_path.clone())
        } else {
            Err(ManError::ConfigFileNotFound(
                user_path.display().to_string(),
            ))
        }
    } else {
        // No -C provided, so check defaults:
        for default in MAN_CONFS {
            let p = PathBuf::from(default);
            if p.exists() {
                return Ok(p);
            }
        }
        Err(ManError::ConfigFileNotFound(
            "No valid man.conf found".to_string(),
        ))
    }
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
            Err(io::Error::other(format!("failed to open stdin for {name}")))?;
        }
    }

    let output = process
        .wait_with_output()
        .map_err(|_| io::Error::other(format!("failed to get {name} stdout")))?;

    if !output.status.success() {
        Err(io::Error::other(format!("{name} failed")))?
    } else {
        Ok(output)
    }
}

/// Gets page width.
///
/// # Returns
///
/// [Option<u16>] width value of current terminal.
/// [Option::Some] if working on terminal and receiving terminal size was succesfull.
/// [Option::None] if working not on terminal.
///
/// # Errors
///
/// Returns [ManError] if working on terminal and failed to get terminal size.
fn get_pager_settings(config: &ManConfig) -> Result<FormattingSettings, ManError> {
    let mut settings = FormattingSettings::default();

    if let Some(Some(val_str)) = config.output_options.get("indent") {
        settings.indent = val_str
            .parse::<usize>()
            .map_err(|err| ManError::ParseError(ParseError::ParseIntError(err)))?;
    }

    if let Some(Some(val_str)) = config.output_options.get("width") {
        settings.width = val_str
            .parse::<usize>()
            .map_err(|err| ManError::ParseError(ParseError::ParseIntError(err)))?;
    }

    // If stdout is not a terminal, don't try to ioctl for size
    if !io::stdout().is_terminal() {
        return Ok(settings);
    }

    // If it is a terminal, try to get the window size via ioctl.
    let mut winsize = libc::winsize {
        ws_row: 0,
        ws_col: 0,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };

    let ret = unsafe { libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &mut winsize) };
    if ret != 0 {
        return Err(ManError::GetTerminalSize);
    }

    // If the terminal is narrower than 79 columns, reduce the width setting
    if winsize.ws_col < 79 {
        settings.width = (winsize.ws_col - 1) as usize;
        // If extremely narrow, reduce indent too
        if winsize.ws_col < 66 {
            settings.indent = 3;
        }
    }

    Ok(settings)
}

/// Read a local man page file (possibly .gz), uncompress if needed, and return
/// the raw content.
fn get_man_page_from_path(path: &PathBuf) -> Result<Vec<u8>, ManError> {
    let ext = path.extension().and_then(|ext| ext.to_str());
    let cat_cmd = match ext {
        Some("gz") => "zcat",
        _ => "cat",
    };

    let output = spawn(cat_cmd, [path], None, Stdio::piped())?;
    Ok(output.stdout)
}

/// Parse and format a man page’s raw content into text suitable for display.
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
fn format_man_page(
    man_bytes: Vec<u8>,
    formatting: &FormattingSettings,
    synopsis: bool,
) -> Result<Vec<u8>, ManError> {
    let content = String::from_utf8(man_bytes)
        .map_err(|err| ManError::ParseError(ParseError::FromUtf8Error(err)))?;

    let mut formatter = MdocFormatter::new(*formatting);

    let document = MdocParser::parse_mdoc(&content)?;
    let formatted_document = match synopsis {
        true => formatter.format_synopsis_section(document),
        false => formatter.format_mdoc(document),
    };

    Ok(formatted_document)
}

/// Write formatted output to either a pager or directly to stdout if `copy = true`.
///
/// # Arguments
///
/// `man_page` - [Vec<u8>] with content that needs to displayed.
///
/// # Errors
///
/// [ManError] if failed to execute pager or failed write to its STDIN.
fn display_pager(man_page: Vec<u8>, copy_mode: bool) -> Result<(), ManError> {
    if copy_mode {
        io::stdout().write_all(&man_page)?;
        io::stdout().flush()?;
        return Ok(());
    }

    let pager = std::env::var("PAGER").unwrap_or_else(|_| "more".to_string());
    let args = if pager.ends_with("more") {
        vec!["-s"]
    } else {
        vec![]
    };

    spawn(&pager, args, Some(&man_page), Stdio::inherit())?;

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
fn display_summary_database(command: &str, keyword: &str) -> Result<bool, ManError> {
    let status = Command::new(command).arg(keyword).spawn()?.wait()?;

    Ok(status.success())
}

/// Man formatting state structure
#[derive(Default)]
struct Man {
    args: Args,
    search_paths: Vec<PathBuf>,
    sections: Vec<Section>,
    config: ManConfig,
    formatting_settings: FormattingSettings,
}

impl Man {
    /// Gets system documentation path by passed name.
    ///
    /// # Arguments
    ///
    /// `name` - [str] name of necessary system documentation.
    ///
    /// # Returns
    ///
    /// [Vec<PathBuf>] of found system documentation.
    ///
    /// # Errors
    ///
    /// [ManError] if file not found.
    fn get_man_page_paths(&self, name: &str, all: bool) -> Result<Vec<PathBuf>, ManError> {
        let mut path_iter = self.search_paths.iter().flat_map(|path| {
            self.sections.iter().flat_map(move |section| {
                let base_path = format!("{}/man{section}/{name}.{section}", path.display());
                vec![format!("{base_path}.gz"), base_path]
            })
        });

        if all {
            let paths = path_iter
                .map(PathBuf::from)
                .filter(|path| path.exists())
                .collect::<Vec<_>>();

            if paths.is_empty() {
                return Err(ManError::PageNotFound(name.to_string()));
            }

            Ok(paths)
        } else {
            path_iter
                .find(|path| PathBuf::from(path).exists())
                .map(|s| vec![PathBuf::from(s)])
                .ok_or_else(|| ManError::PageNotFound(name.to_string()))
        }
    }

    /// Display a single man page found at `path`.
    ///
    /// # Arguments
    ///
    /// `name` - [str] name of system documentation.
    ///
    /// # Errors
    ///
    /// [ManError] if man page not found, or any display error happened.
    fn display_man_page(&self, path: &PathBuf) -> Result<(), ManError> {
        let raw = get_man_page_from_path(path)?;
        let formatted = format_man_page(raw, &self.formatting_settings, self.args.synopsis)?;
        display_pager(formatted, self.args.copy)
    }

    /// Display *all* man pages found for a particular name (when -a is specified).
    fn display_all_man_pages(&self, paths: Vec<PathBuf>) -> Result<(), ManError> {
        if paths.is_empty() {
            return Err(ManError::PageNotFound("no matching pages".to_string()));
        }

        if paths.iter().any(|path| !path.exists()) {
            return Err(ManError::PageNotFound(
                "One of the provided files was not found".to_string(),
            ));
        }

        for path in paths {
            self.display_man_page(&path)?;
        }

        Ok(())
    }

    /// Display *all* man page pathes (when -w is specified).
    fn display_paths(&self, paths: Vec<PathBuf>) -> Result<(), ManError> {
        if paths.is_empty() {
            return Err(ManError::PageNotFound("no matching pages".to_string()));
        }

        if paths.iter().any(|path| !path.exists()) {
            return Err(ManError::PageNotFound(
                "One of the provided files was not found".to_string(),
            ));
        }

        for path in paths {
            println!("{}", path.display());
        }

        Ok(())
    }

    fn new(args: Args) -> Result<Self, ManError> {
        if args.names.is_empty() {
            if args.local_file.is_none() {
                return Err(ManError::NoNames);
            }

            for path in args.local_file.clone().unwrap() {
                if !path.exists() {
                    return Err(ManError::NotFound(path));
                }
            }
        }

        let config_path = get_config_file_path(&args.config_file)?;
        let config = parse_config_file(config_path)?;

        let mut man = Self {
            args,
            formatting_settings: get_pager_settings(&config)?,
            config,
            ..Default::default()
        };

        if !man.args.override_paths.is_empty() {
            let override_paths = man
                .args
                .override_paths
                .iter()
                .filter_map(|path| path.to_str())
                .collect::<Vec<_>>()
                .join(":");

            std::env::set_var("MANPATH", OsStr::new(&override_paths));
        }

        if man.args.subsection.is_some() {
            std::env::set_var("MACHINE", OsStr::new(&man.args.subsection.clone().unwrap()));
        }

        let manpath = std::env::var("MANPATH")
            .unwrap_or_default()
            .split(":")
            .filter_map(|s| PathBuf::from_str(s).ok())
            .collect::<Vec<_>>();

        man.search_paths = [
            man.args.augment_paths.clone(),
            manpath,
            man.search_paths.clone(),
            man.config.manpaths.clone(),
            MAN_PATHS
                .iter()
                .filter_map(|s| PathBuf::from_str(s).ok())
                .collect::<Vec<_>>(),
        ]
        .concat();

        if man.search_paths.is_empty() {
            return Err(ManError::ManPaths);
        }

        man.sections = if let Some(section) = man.args.section {
            vec![section]
        } else {
            MAN_SECTIONS.to_vec()
        };

        Ok(man)
    }

    //
    // ──────────────────────────────────────────────────────────────────────────────
    //  MAIN LOGIC FUNCTION
    // ──────────────────────────────────────────────────────────────────────────────
    //

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
    fn man(&mut self) -> Result<bool, ManError> {
        let mut no_errors = true;

        if let Some(paths) = &self.args.local_file {
            if self.args.list_pathnames {
                let paths = paths
                    .iter()
                    .filter(|path| path.exists())
                    .cloned()
                    .collect::<Vec<_>>();
                self.display_paths(paths)?;
            } else {
                self.display_all_man_pages(paths.clone())?;
            }
            return Ok(no_errors);
        } else if self.args.apropos || self.args.whatis {
            let command = if self.args.apropos {
                "apropos"
            } else {
                "whatis"
            };

            for keyword in &self.args.names {
                let success = display_summary_database(command, keyword)?;
                if !success {
                    no_errors = false;
                }
            }

            return Ok(no_errors);
        }

        for name in &self.args.names {
            if self.args.list_pathnames {
                let paths = self.get_man_page_paths(name, true)?;
                self.display_paths(paths)?;
            } else {
                let paths = self.get_man_page_paths(name, self.args.all)?;
                self.display_all_man_pages(paths)?;
            }
        }

        Ok(no_errors)
    }
}

//
// ──────────────────────────────────────────────────────────────────────────────
//  MAIN ENTRY POINT
// ──────────────────────────────────────────────────────────────────────────────
//

// Exit code:
//     0 - Successful completion.
//     >0 - An error occurred.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    // parse command line arguments
    let args = Args::parse();

    let mut man = match Man::new(args) {
        Ok(man) => man,
        Err(err) => {
            eprintln!("man: {err}");
            std::process::exit(1);
        }
    };

    // Run main logic
    let exit_code = match man.man() {
        // Success, all pages displayed or apropos found something
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
