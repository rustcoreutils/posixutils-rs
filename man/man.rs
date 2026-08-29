//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::{ArgAction, Parser, ValueEnum};
use flate2::read::GzDecoder;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use man_util::config::{parse_config_file, ManConfig};
use man_util::formatter::MdocFormatter;
use man_util::man7;
use man_util::parse::mdoc::NestingTooDeep;
use man_util::parser::{MdocDocument, MdocParser};
use std::ffi::OsStr;
use std::io::{self, IsTerminal, Read, Write};
use std::num::ParseIntError;
use std::path::{Component, Path, PathBuf};
use std::process::{Command, Stdio};
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
///
/// `Display` is written by hand rather than derived: POSIX lists `LC_MESSAGES`
/// as affecting `man`, and a `thiserror` `#[error("…")]` string is a
/// compile-time literal that cannot be routed through `gettext`.
#[derive(Debug)]
enum ManError {
    /// Search path to man pages isn't exists
    ManPaths,

    /// Commands for searching documentation isn't exists
    NoNames,

    /// Man can't find documentation for choosen command
    PageNotFound(String),

    /// Configuration file was not found
    ConfigFileNotFound(String),

    /// Can't get terminal size
    GetTerminalSize,

    /// Man can't find choosen command
    CommandNotFound(String),

    /// Can't execute command; read/write file
    Io(io::Error),

    /// Parsing error
    ParseError(ParseError),

    /// Not found error
    NotFound(PathBuf),

    /// The page produced no renderable content (e.g. an unsupported format).
    EmptyPage,
}

impl std::fmt::Display for ManError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ManError::ManPaths => write!(f, "{}", gettext("man paths to man pages doesn't exist")),
            ManError::NoNames => write!(f, "{}", gettext("no names specified")),
            ManError::PageNotFound(name) => write!(
                f,
                "{}",
                gettext("system documentation for \"{}\" not found").replace("{}", name)
            ),
            ManError::ConfigFileNotFound(path) => write!(
                f,
                "{}",
                gettext("configuration file was not found: {}").replace("{}", path)
            ),
            ManError::GetTerminalSize => write!(f, "{}", gettext("failed to get terminal size")),
            ManError::CommandNotFound(cmd) => {
                write!(f, "{}", gettext("{} command not found").replace("{}", cmd))
            }
            ManError::Io(err) => write!(
                f,
                "{}",
                gettext("failed to execute command: {}").replace("{}", &err.to_string())
            ),
            ManError::ParseError(err) => write!(
                f,
                "{}",
                gettext("parsing error: {}").replace("{}", &err.to_string())
            ),
            ManError::NotFound(path) => write!(
                f,
                "{}",
                gettext("file: {} was not found").replace("{}", &path.display().to_string())
            ),
            ManError::EmptyPage => write!(f, "{}", gettext("no renderable content in page")),
        }
    }
}

impl std::error::Error for ManError {}

impl From<io::Error> for ManError {
    fn from(err: io::Error) -> Self {
        ManError::Io(err)
    }
}

impl From<ParseError> for ManError {
    fn from(err: ParseError) -> Self {
        ManError::ParseError(err)
    }
}

impl From<NestingTooDeep> for ManError {
    fn from(err: NestingTooDeep) -> Self {
        ManError::ParseError(err.into())
    }
}

/// Parsing error types
#[derive(Error, Debug)]
enum ParseError {
    #[error("{0}")]
    ParseIntError(#[from] ParseIntError),

    #[error("{0}")]
    FromUtf8Error(#[from] FromUtf8Error),

    #[error("{0}")]
    NestingTooDeep(#[from] NestingTooDeep),
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
    /// Whether to emit bold/underline via nroff backspace-overstrike. Enabled
    /// when the output goes to an interactive terminal; off when piped so the
    /// text stays clean for `grep`/redirection.
    pub styling: bool,
}

impl Default for FormattingSettings {
    fn default() -> Self {
        Self {
            width: 78,
            indent: 6,
            styling: false,
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
    // Emphasis (overstrike) is only emitted for an interactive terminal, so
    // piped/redirected output stays plain text.
    let mut settings = FormattingSettings {
        styling: io::stdout().is_terminal(),
        ..FormattingSettings::default()
    };

    if let Some(Some(val_str)) = config.output_options.get("indent") {
        settings.indent = val_str
            .parse::<usize>()
            .map_err(|err| ManError::ParseError(ParseError::ParseIntError(err)))?;
    }

    let config_width = match config.output_options.get("width") {
        Some(Some(val_str)) => Some(
            val_str
                .parse::<usize>()
                .map_err(|err| ManError::ParseError(ParseError::ParseIntError(err)))?,
        ),
        _ => None,
    };

    // Width precedence: COLUMNS (a per-invocation override, honored even when
    // piped) > an explicit config width > the terminal's own size (when stdout
    // is a tty) > the default width (78). A one-column right margin is kept when
    // deriving from a column count.
    if let Some(cols) = std::env::var("COLUMNS")
        .ok()
        .and_then(|c| c.trim().parse::<usize>().ok())
    {
        apply_terminal_width(&mut settings, cols);
        return Ok(settings);
    }

    if let Some(width) = config_width {
        settings.width = width;
        return Ok(settings);
    }

    if !io::stdout().is_terminal() {
        return Ok(settings);
    }

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

    apply_terminal_width(&mut settings, winsize.ws_col as usize);

    Ok(settings)
}

/// Set the formatting width from a terminal column count, keeping a one-column
/// right margin and narrowing the indent on very small terminals. A reported
/// width of 0 (or 1) leaves the default in place rather than underflowing.
fn apply_terminal_width(settings: &mut FormattingSettings, cols: usize) {
    if cols < 2 {
        return;
    }
    settings.width = cols - 1;
    if cols < 66 {
        settings.indent = 3;
    }
}

/// Decode a page image, transparently gunzipping it.
///
/// Detection is by gzip magic rather than by a `.gz` suffix: the previous
/// implementation chose `zcat` or `cat` from the file extension, so a
/// compressed page stored under any other name was written to the terminal as
/// binary. At most `limit` decoded bytes are produced, which also bounds a
/// decompression bomb.
fn decode_page(raw: Vec<u8>, limit: u64) -> Result<Vec<u8>, ManError> {
    // Compressed formats we do not implement. Installed pages are gzip
    // everywhere this runs; recognising the others turns "binary dumped to the
    // terminal" into a clean diagnostic.
    for magic in [
        b"BZh".as_slice(),
        b"\xfd7zXZ".as_slice(),
        b"\x28\xb5\x2f\xfd".as_slice(),
    ] {
        if raw.starts_with(magic) {
            return Err(ManError::EmptyPage);
        }
    }

    if !raw.starts_with(&[0x1f, 0x8b]) {
        return Ok(raw);
    }

    let mut out = Vec::new();
    GzDecoder::new(raw.as_slice())
        .take(limit)
        .read_to_end(&mut out)?;
    Ok(out)
}

/// Read a local man page file (possibly .gz), uncompress if needed, and return
/// the raw content.
fn get_man_page_from_path(path: &Path) -> Result<Vec<u8>, ManError> {
    decode_page(std::fs::read(path)?, u64::MAX)
}

/// Decoded bytes read from a page when only its NAME section is wanted, which
/// is always near the top. The keyword scan visits every installed page, and
/// decoding each one in full is the bulk of its cost.
const PAGE_HEAD_BYTES: u64 = 64 << 10;

/// Read at most [`PAGE_HEAD_BYTES`] of a page, for the keyword scan.
fn read_page_head(path: &Path) -> Option<Vec<u8>> {
    decode_page(std::fs::read(path).ok()?, PAGE_HEAD_BYTES).ok()
}

/// Whether a roff `.so` target is safe to resolve.
///
/// A man page is untrusted input — it may arrive from a package, a shared
/// `MANPATH`, or `man -l` on a downloaded file — so `.so` must not be usable to
/// read arbitrary files and render them to the terminal. `.so /etc/passwd` did
/// exactly that. mandoc's rule, adopted here: the target must be relative and
/// must not walk upwards.
fn is_safe_so_target(target: &str) -> bool {
    let path = Path::new(target);
    !path.is_absolute()
        && !path
            .components()
            .any(|c| matches!(c, Component::ParentDir | Component::RootDir))
}

/// Resolve a roff `.so` include target to its (decompressed) text for the roff
/// front-end. Tries the target as given, then under each system man root, with
/// and without a `.gz` suffix. Returns `None` if nothing readable is found.
fn load_so(target: &str) -> Option<String> {
    if !is_safe_so_target(target) {
        return None;
    }
    let mut candidates: Vec<PathBuf> = vec![PathBuf::from(target)];
    for root in MAN_PATHS {
        candidates.push(PathBuf::from(root).join(target));
    }
    for cand in candidates {
        let gz = PathBuf::from(format!("{}.gz", cand.display()));
        for path in [cand, gz] {
            if path.is_file() {
                if let Ok(bytes) = get_man_page_from_path(&path) {
                    return Some(String::from_utf8(bytes).unwrap_or_else(|err| {
                        err.into_bytes().iter().map(|&b| b as char).collect()
                    }));
                }
            }
        }
    }
    None
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
    // Most pages are UTF-8; a page that is not (e.g. Latin-1) is decoded
    // byte-for-byte into the Latin-1 Unicode block rather than rejected, so it
    // still renders instead of erroring out.
    let content = String::from_utf8(man_bytes)
        .unwrap_or_else(|err| err.into_bytes().iter().map(|&b| b as char).collect());

    // Run the roff front-end first: execute roff programmability (registers,
    // conditionals, user macros, `.so` includes) and normalize the stream before
    // language detection and parsing. A page without roff programmability is
    // returned essentially unchanged.
    // The available width (terminal less the base indent) reaches the roff pass
    // only for tbl, which must choose a fill width for `T{`…`T}` text blocks
    // before it can lay them out.
    let line_length = formatting.width.saturating_sub(formatting.indent);
    let content = man_util::roff::preprocess_with_loader(&content, line_length, load_so);

    // Legacy man(7) pages (`.TH`/`.SH`/…) are handled by a dedicated renderer;
    // the mdoc engine only understands mdoc(7) and would otherwise emit an empty
    // page. The synopsis-only mode (-h) is mdoc-specific.
    if !synopsis && man7::is_man7(&content) {
        return man7::format_man7(&content, formatting).ok_or(ManError::EmptyPage);
    }

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
    // POSIX: the output is piped through PAGER only "When standard output is a
    // terminal device." With `-c`, or when stdout is not a terminal (e.g.
    // `man foo | grep bar`), write directly to stdout instead.
    if copy_mode || !io::stdout().is_terminal() {
        io::stdout().write_all(&man_page)?;
        io::stdout().flush()?;
        return Ok(());
    }

    let cmd = pager_command(std::env::var("PAGER").ok().as_deref());

    let mut child = Command::new("sh")
        .arg("-c")
        .arg(&cmd)
        .stdin(Stdio::piped())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|err| match err.kind() {
            io::ErrorKind::NotFound => ManError::CommandNotFound("sh".to_string()),
            _ => ManError::Io(err),
        })?;

    // The pager exits as soon as the user quits, so writing the page into it
    // races with that exit. Rust ignores SIGPIPE, so the write returns EPIPE
    // instead of killing us -- that is the user pressing `q`, not a failure.
    if let Some(mut sink) = child.stdin.take() {
        match sink.write_all(&man_page).and_then(|()| sink.flush()) {
            Ok(()) => {}
            Err(err) if err.kind() == io::ErrorKind::BrokenPipe => {
                let _ = child.wait();
                return Ok(());
            }
            Err(err) => return Err(err.into()),
        }
        // `sink` drops here, closing the pipe so the pager sees EOF. It must
        // drop before the wait below, or the two deadlock.
    }

    // sh's convention: 127 is "not found", 126 is "found but not executable".
    // Any other status belongs to the pager, and a user who quits it has not
    // made `man` fail.
    match child.wait()?.code() {
        Some(126) | Some(127) => Err(ManError::CommandNotFound(cmd)),
        _ => Ok(()),
    }
}

/// The command string to page output through.
///
/// POSIX: PAGER is "any string acceptable as a command_string operand to the
/// `sh -c` command", so it is handed to the shell verbatim rather than executed
/// as a program name -- `PAGER="less -R"` is a conforming value that used to
/// fail with "less -R command not found". A null PAGER is spec-equivalent to an
/// unset one; `env::var` returns Ok("") for it, which produced the empty
/// "  command not found".
///
/// A whitespace-only value is treated as unset too: it is not strictly null,
/// but `sh -c " "` discards the page and reports success, which no user means.
fn pager_command(pager: Option<&str>) -> String {
    match pager {
        Some(p) if !p.trim().is_empty() => p.to_string(),
        // `-s` belongs to the paginator we chose, not to an arbitrary user
        // command: appending it to one would corrupt any command with its own
        // quoting or a pipeline.
        _ => "more -s".to_string(),
    }
}

/// Extracts NAME section info from a parsed mdoc document.
///
/// Returns (names, description) where names is the list of command names
/// from .Nm macros and description is the text from .Nd macro.
fn extract_name_info(document: &MdocDocument) -> Option<(Vec<String>, String)> {
    use man_util::mdoc_macro::Macro;
    use man_util::parser::Element;

    let mut names: Vec<String> = Vec::new();
    let mut description = String::new();
    let mut in_name_section = false;

    for element in &document.elements {
        if let Element::Macro(node) = element {
            match &node.mdoc_macro {
                Macro::Sh { title } => {
                    in_name_section = title.eq_ignore_ascii_case("NAME");
                }
                Macro::Nm { name: Some(n) } if in_name_section => {
                    if !n.is_empty() && !names.contains(n) {
                        names.push(n.clone());
                    }
                }
                Macro::Nd if in_name_section => {
                    // Collect text from child nodes
                    for child in &node.nodes {
                        if let Element::Text(text) = child {
                            if !description.is_empty() {
                                description.push(' ');
                            }
                            description.push_str(text.trim());
                        }
                    }
                }
                _ => {}
            }
        }
    }

    if names.is_empty() && description.is_empty() {
        None
    } else {
        Some((names, description))
    }
}

/// Information about a man page for keyword search.
struct ManPageInfo {
    names: Vec<String>,
    description: String,
    section: String,
}

/// Scans man page directories and extracts NAME section info from all pages.
fn scan_man_pages(search_paths: &[PathBuf], sections: &[Section]) -> Vec<ManPageInfo> {
    let mut results = Vec::new();

    for search_path in search_paths {
        for section in sections {
            let section_str = section.to_string();
            let section_dir = search_path.join(format!("man{}", section_str));

            if !section_dir.is_dir() {
                continue;
            }

            let entries = match std::fs::read_dir(&section_dir) {
                Ok(e) => e,
                Err(_) => continue,
            };

            for entry in entries.flatten() {
                let path = entry.path();
                if !path.is_file() {
                    continue;
                }

                // Try to read and parse the man page
                let Some(raw) = read_page_head(&path) else {
                    continue;
                };

                // Lossy, not strict: a page that is not valid UTF-8 was skipped
                // outright, even though the display path renders it by decoding
                // it as Latin-1.
                let content = String::from_utf8_lossy(&raw);

                // A single pathologically nested page must not abort the scan.
                let Ok(document) = MdocParser::parse_mdoc(&content) else {
                    continue;
                };

                if let Some((names, description)) = extract_name_info(&document) {
                    results.push(ManPageInfo {
                        names,
                        description,
                        section: section_str.clone(),
                    });
                }
            }
        }
    }

    results
}

/// Performs native keyword search across man pages.
///
/// Returns lines in format: "command(section) - description"
fn native_keyword_search(
    search_paths: &[PathBuf],
    sections: &[Section],
    keywords: &[String],
) -> Vec<String> {
    let pages = scan_man_pages(search_paths, sections);
    let mut results = Vec::new();

    // POSIX: `-k` keywords are case-insensitive extended regular expressions
    // (the spec describes the search as equivalent to `grep -Ei`). Use the
    // project's POSIX ERE engine (libc regcomp via `plib::regex`) so the
    // semantics match the spec, not Rust-regex syntax; fall back to a literal
    // substring match if the keyword is not valid ERE syntax.
    use plib::regex::{Regex, RegexFlags};
    enum Matcher {
        Regex(Regex),
        Literal(String),
    }
    impl Matcher {
        fn is_match(&self, haystack: &str) -> bool {
            match self {
                Matcher::Regex(re) => re.is_match(haystack),
                Matcher::Literal(lit) => haystack.to_lowercase().contains(lit),
            }
        }
    }
    let matchers: Vec<Matcher> = keywords
        .iter()
        .map(|kw| match Regex::new(kw, RegexFlags::ere().ignore_case()) {
            Ok(re) => Matcher::Regex(re),
            Err(_) => Matcher::Literal(kw.to_lowercase()),
        })
        .collect();

    for page in &pages {
        // Check if any keyword matches name or description (case-insensitive ERE)
        let matches = matchers
            .iter()
            .any(|m| page.names.iter().any(|n| m.is_match(n)) || m.is_match(&page.description));

        if matches {
            // Format output: "name(section) - description"
            for name in &page.names {
                results.push(format!("{}({}) - {}", name, page.section, page.description));
            }
        }
    }

    // Sort results alphabetically
    results.sort();
    // Remove duplicates
    results.dedup();

    results
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
    fn display_man_page(&self, path: &Path) -> Result<(), ManError> {
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
            // Try external apropos/whatis first (uses pre-built database, fast)
            // Fall back to native search if external command not available
            let command = if self.args.apropos {
                "apropos"
            } else {
                "whatis"
            };

            // Check if external command exists and use it
            let external_available = Command::new("which")
                .arg(command)
                .output()
                .map(|o| o.status.success())
                .unwrap_or(false);

            if external_available {
                // Use external command (fast, uses system database)
                for keyword in &self.args.names {
                    let status = Command::new(command).arg(keyword).spawn()?.wait()?;
                    if !status.success() {
                        no_errors = false;
                    }
                }
            } else {
                // Fall back to native keyword search (slower, no database)
                let results =
                    native_keyword_search(&self.search_paths, &self.sections, &self.args.names);

                if results.is_empty() {
                    for keyword in &self.args.names {
                        eprintln!("{}: {}", keyword, gettext("nothing appropriate"));
                    }
                    no_errors = false;
                } else {
                    for line in results {
                        println!("{}", line);
                    }
                }
            }

            return Ok(no_errors);
        }

        for name in &self.args.names {
            // A failure for one operand (e.g. page not found) is reported but
            // does not abort the remaining operands; the exit status is still
            // non-zero overall.
            let result = if self.args.list_pathnames {
                self.get_man_page_paths(name, true)
                    .and_then(|paths| self.display_paths(paths))
            } else {
                self.get_man_page_paths(name, self.args.all)
                    .and_then(|paths| self.display_all_man_pages(paths))
            };

            if let Err(err) = result {
                eprintln!("man: {err}");
                no_errors = false;
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

#[cfg(test)]
mod tests {
    use super::{decode_page, is_safe_so_target, pager_command, ManError};

    #[test]
    fn pager_falls_back_to_more_when_unset_or_null() {
        // POSIX: "If the PAGER variable is null or not set, the command shall
        // be either more or another paginator." env::var returns Ok("") for a
        // null PAGER, which produced `man:  command not found`.
        assert_eq!(pager_command(None), "more -s");
        assert_eq!(pager_command(Some("")), "more -s");
        assert_eq!(pager_command(Some("   ")), "more -s");
    }

    #[test]
    fn pager_keeps_a_command_string_intact() {
        // "Any string acceptable as a command_string operand to the sh -c
        // command shall be valid." This was executed as a program name, so a
        // conforming value failed with `less -R command not found`. Note no
        // `-s` is appended: that belongs to our chosen default, not to a user
        // command that may carry its own quoting or a pipeline.
        assert_eq!(pager_command(Some("less -R")), "less -R");
        assert_eq!(
            pager_command(Some("sed s/a/b/ | more")),
            "sed s/a/b/ | more"
        );
    }

    #[test]
    fn decode_page_detects_gzip_by_magic_not_extension() {
        // The decoder used to be chosen from the file extension, so a
        // compressed page stored under any other name was written to the
        // terminal as binary.
        use flate2::write::GzEncoder;
        use flate2::Compression;
        use std::io::Write;

        let mut enc = GzEncoder::new(Vec::new(), Compression::default());
        enc.write_all(b".TH T 1\n").unwrap();
        let gz = enc.finish().unwrap();

        assert_eq!(decode_page(gz, u64::MAX).unwrap(), b".TH T 1\n");
    }

    #[test]
    fn decode_page_passes_plain_text_through() {
        let raw = b".TH T 1\n.SH NAME\n".to_vec();
        assert_eq!(decode_page(raw.clone(), u64::MAX).unwrap(), raw);
    }

    #[test]
    fn decode_page_rejects_compression_it_cannot_read() {
        // Better a diagnostic than binary on the terminal.
        for magic in [b"BZh9".as_slice(), b"\xfd7zXZ\x00".as_slice()] {
            assert!(matches!(
                decode_page(magic.to_vec(), u64::MAX),
                Err(ManError::EmptyPage)
            ));
        }
    }

    /// `.so` must not be usable to read arbitrary files. A man page is
    /// untrusted input, and `.so /etc/passwd` rendered the password file to the
    /// terminal with exit status 0.
    #[test]
    fn so_target_must_be_relative_and_not_escape() {
        assert!(!is_safe_so_target("/etc/passwd"));
        assert!(!is_safe_so_target("/proc/self/environ"));
        assert!(!is_safe_so_target("../../../etc/shadow"));
        assert!(!is_safe_so_target("man1/../../../etc/passwd"));
        assert!(!is_safe_so_target(".."));

        // Ordinary alias targets, which is what `.so` is actually for.
        assert!(is_safe_so_target("man1/ls.1"));
        assert!(is_safe_so_target("man3/printf.3"));
        assert!(is_safe_so_target("./man1/ls.1"));
    }

    /// Every `ManError` message routes through `gettext`, so `LC_MESSAGES` can
    /// translate it once catalogs are loaded. `thiserror`'s `#[error("…")]` is a
    /// compile-time literal and cannot; the derive was therefore replaced with a
    /// hand-written `Display`, and this pins the resulting text so the rewrite
    /// did not change any diagnostic.
    #[test]
    fn error_messages_are_unchanged_and_translatable() {
        use std::path::PathBuf;

        let cases: Vec<(ManError, &str)> = vec![
            (ManError::ManPaths, "man paths to man pages doesn't exist"),
            (ManError::NoNames, "no names specified"),
            (
                ManError::PageNotFound("ls".into()),
                "system documentation for \"ls\" not found",
            ),
            (
                ManError::ConfigFileNotFound("/etc/man.conf".into()),
                "configuration file was not found: /etc/man.conf",
            ),
            (ManError::GetTerminalSize, "failed to get terminal size"),
            (
                ManError::CommandNotFound("zcat".into()),
                "zcat command not found",
            ),
            (
                ManError::NotFound(PathBuf::from("/tmp/x.1")),
                "file: /tmp/x.1 was not found",
            ),
            (ManError::EmptyPage, "no renderable content in page"),
        ];

        for (err, expected) in cases {
            assert_eq!(err.to_string(), expected);
        }

        // The two wrapping variants keep their source's text.
        let io = ManError::Io(std::io::Error::other("boom"));
        assert_eq!(io.to_string(), "failed to execute command: boom");
    }
}
