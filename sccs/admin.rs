//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! admin - create and administer SCCS files

use std::fs::{self, File};
use std::io::{self, BufRead, BufReader};
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::sccsfile::{
    paths, BodyRecord, DeltaEntry, DeltaStats, DeltaType, SccsDateTime, SccsFile, SccsFlag,
    SccsHeader, Sid, ZLock,
};

/// admin - create and administer SCCS files
#[derive(Parser)]
#[command(version, about = gettext("admin - create and administer SCCS files"), disable_help_flag = true)]
struct Args {
    #[arg(long = "help", help = gettext("Print help information"))]
    help: bool,

    #[arg(short = 'n', help = gettext("Create a new SCCS file"))]
    new_file: bool,

    #[arg(short = 'r', value_name = "SID", help = gettext("Initial SID for new file"))]
    initial_sid: Option<String>,

    #[arg(short = 'm', value_name = "MRLIST", help = gettext("MR numbers for initial delta"))]
    mrlist: Option<String>,

    #[arg(short = 'f', value_name = "FLAG", help = gettext("Set flag (can be repeated)"))]
    set_flags: Vec<String>,

    #[arg(short = 'd', value_name = "FLAG", help = gettext("Delete flag (can be repeated)"))]
    delete_flags: Vec<String>,

    #[arg(short = 'a', value_name = "LOGIN", help = gettext("Add user to authorized list"))]
    add_users: Vec<String>,

    #[arg(short = 'e', value_name = "LOGIN", help = gettext("Remove user from authorized list"))]
    erase_users: Vec<String>,

    #[arg(short = 'h', help = gettext("Check file structure and checksum"))]
    check: bool,

    #[arg(short = 'z', help = gettext("Recompute and store checksum"))]
    recompute_checksum: bool,

    #[arg(required = true, help = gettext("SCCS files to process"))]
    files: Vec<PathBuf>,
}

/// Attached-only optional-value options (`-i`, `-t`, `-y`).
///
/// POSIX requires the optional option-arguments for these options to be
/// presented attached to the option letter (e.g. `-iinput.txt`), never as a
/// separate following operand. clap cannot express "attached value only", so we
/// pre-scan argv and strip these tokens before handing the rest to clap.
///
/// Each field is:
/// - `None`         => option not given;
/// - `Some(None)`   => bare option given (read stdin / clear text / default
///   comment);
/// - `Some(Some(s))` => attached value `s`.
#[derive(Default)]
struct AttachedOpts {
    init_file: Option<Option<String>>,
    desc_text: Option<Option<String>>,
    comment: Option<Option<String>>,
}

/// Split argv into the attached-only options (`-i`/`-t`/`-y`) and the remaining
/// arguments to be parsed by clap. Recognizes the bare and attached forms; a
/// bare option never consumes the following argument. Option bundling for these
/// is not supported (historical admin does not bundle them either).
fn extract_attached_opts<I>(argv: I) -> (AttachedOpts, Vec<String>)
where
    I: IntoIterator<Item = String>,
{
    let mut opts = AttachedOpts::default();
    let mut rest = Vec::new();

    for arg in argv {
        let slot = match arg.as_str() {
            "-i" => Some((&mut opts.init_file, None)),
            "-t" => Some((&mut opts.desc_text, None)),
            "-y" => Some((&mut opts.comment, None)),
            _ if arg.starts_with("-i") => Some((&mut opts.init_file, Some(arg[2..].to_string()))),
            _ if arg.starts_with("-t") => Some((&mut opts.desc_text, Some(arg[2..].to_string()))),
            _ if arg.starts_with("-y") => Some((&mut opts.comment, Some(arg[2..].to_string()))),
            _ => None,
        };

        match slot {
            Some((field, value)) => *field = Some(value),
            None => rest.push(arg),
        }
    }

    (opts, rest)
}

/// Split a `-m` mrlist option-argument into individual MR numbers.
fn attached_mr_list(mrlist: Option<&str>) -> Vec<String> {
    match mrlist {
        Some(s) => s.split_whitespace().map(String::from).collect(),
        None => Vec::new(),
    }
}

fn get_username() -> String {
    plib::sccsfile::real_login_name()
}

fn read_input_file(path: Option<&str>) -> io::Result<Vec<u8>> {
    let mut reader: Box<dyn BufRead> = match path {
        Some(p) => Box::new(BufReader::new(File::open(p)?)),
        None => Box::new(BufReader::new(io::stdin())),
    };
    let mut data = Vec::new();
    reader.read_to_end(&mut data)?;
    Ok(data)
}

/// Whether the initial content must be uuencoded (the `e` flag): non-empty
/// input that is binary (invalid UTF-8 or contains NUL/SOH, which would corrupt
/// the s-file format) or lacks a trailing newline — matching CSSC's trigger.
fn content_needs_encoding(data: &[u8]) -> bool {
    if data.is_empty() {
        return false;
    }
    if !data.ends_with(b"\n") {
        return true;
    }
    std::str::from_utf8(data).is_err() || data.iter().any(|&b| b == 0x00 || b == 0x01)
}

fn read_desc_text(path: Option<&str>) -> io::Result<Vec<String>> {
    match path {
        Some(p) => {
            let content = fs::read_to_string(p)?;
            Ok(content.lines().map(String::from).collect())
        }
        None => Ok(Vec::new()),
    }
}

fn parse_sid(s: &str) -> Result<Sid, String> {
    s.parse::<Sid>().map_err(|e| e.to_string())
}

/// Flag letters recognized by the `-f` option (POSIX admin spec).
const VALID_FLAG_CHARS: &[char] = &[
    'b', 'c', 'd', 'e', 'f', 'i', 'j', 'l', 'm', 'n', 'q', 't', 'v',
];

fn parse_flag(flag_str: &str) -> Result<SccsFlag, String> {
    if flag_str.is_empty() {
        return Err(gettext("Empty flag"));
    }

    let flag_char = flag_str.chars().next().unwrap();
    let value = if flag_str.len() > 1 {
        &flag_str[1..]
    } else {
        ""
    };

    // Reject any flag letter not in the spec set. SccsFlag::parse maps unknown
    // letters to SccsFlag::Unknown for lossless reads of existing files, but
    // admin must not write a malformed flag from a bad -f argument.
    if !VALID_FLAG_CHARS.contains(&flag_char) {
        return Err(format!("{} '{}'", gettext("Unrecognized flag"), flag_char));
    }

    let flag = SccsFlag::parse(flag_char, value).map_err(|e| e.to_string())?;

    // Spec caps ceiling/floor at 9999.
    match &flag {
        SccsFlag::Ceiling(n) | SccsFlag::Floor(n) if *n > 9999 => {
            return Err(format!(
                "{} '{}' {}",
                gettext("value"),
                n,
                gettext("out of range (max 9999)")
            ));
        }
        _ => {}
    }

    Ok(flag)
}

/// Whether `data` contains an SCCS identification keyword of the form `%X%`
/// where X is an uppercase ASCII letter (matching get/delta's keyword set).
fn content_has_id_keyword(data: &[u8]) -> bool {
    let mut i = 0;
    while i + 2 < data.len() {
        if data[i] == b'%' && data[i + 1].is_ascii_uppercase() && data[i + 2] == b'%' {
            return true;
        }
        i += 1;
    }
    false
}

/// Inputs for creating a new SCCS file.
struct NewFileParams<'a> {
    content: Vec<u8>,
    initial_sid: Option<&'a str>,
    comment: Option<&'a str>,
    desc_text: Vec<String>,
    flags: Vec<SccsFlag>,
    users: Vec<String>,
    mr_numbers: Vec<String>,
}

/// Acquire the per-command z-file lock, mapping the "already locked" case to a
/// clear diagnostic so the caller's `Err` arm reports the s-file is being
/// edited rather than a raw "File exists".
fn acquire_zlock(path: &Path) -> io::Result<ZLock> {
    ZLock::acquire(path).map_err(|e| {
        if e.kind() == io::ErrorKind::AlreadyExists {
            io::Error::new(
                io::ErrorKind::AlreadyExists,
                gettext("being edited (z-file lock held)"),
            )
        } else {
            e
        }
    })
}

/// Write `serialized` to the x-file, apply `perms`, and atomically rename over
/// `path`. The x-file is registered for SIGINT cleanup for the duration of the
/// write+rename, and removed on error.
fn write_xfile_atomic(
    path: &Path,
    x_file: &Path,
    serialized: &[u8],
    perms: fs::Permissions,
) -> io::Result<()> {
    plib::sccsfile::register_cleanup(x_file);
    let res = (|| -> io::Result<()> {
        fs::write(x_file, serialized)?;
        fs::set_permissions(x_file, perms)?;
        fs::rename(x_file, path)?;
        Ok(())
    })();
    plib::sccsfile::unregister_cleanup(x_file);
    if res.is_err() {
        let _ = fs::remove_file(x_file);
    }
    res
}

fn create_new_sccs_file(path: &Path, params: NewFileParams) -> io::Result<()> {
    // Per-command z-file lock around the create.
    let _zlock = acquire_zlock(path)?;

    let NewFileParams {
        content,
        initial_sid,
        comment,
        desc_text,
        mut flags,
        users,
        mr_numbers,
    } = params;

    // Determine initial SID
    let sid = match initial_sid {
        Some(s) => parse_sid(s).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?,
        None => Sid::trunk(1, 1),
    };

    // Validate: must be trunk SID
    if !sid.is_trunk() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            gettext("Initial SID must be a trunk SID (no branch component)"),
        ));
    }

    let now = SccsDateTime::now();
    let user = get_username();

    // Create default comment if not provided
    let comment_text = match comment {
        Some(c) => c.to_string(),
        None => format!(
            "date and time created {} {} by {}",
            now.date_string(),
            now.time_string(),
            user
        ),
    };

    // Decide whether to store the body uuencoded (binary / no trailing newline).
    let encoded = content_needs_encoding(&content);

    // Warn (non-fatal) when a text body has no SCCS id keyword, matching cssc.
    if !encoded && !content_has_id_keyword(&content) {
        eprintln!(
            "admin: {}: {}: {}",
            gettext("warning"),
            path.display(),
            gettext("No id keywords.")
        );
    }

    let body_lines: Vec<String> = if encoded {
        flags.push(SccsFlag::Encoded(1));
        plib::sccsfile::uuencode_sccs(&content)
    } else {
        // Plain text: split into lines (drops the trailing newline, no final
        // empty element), matching historical line-oriented storage.
        String::from_utf8_lossy(&content)
            .lines()
            .map(String::from)
            .collect()
    };

    // Calculate stats. For an encoded body the inserted count is the number of
    // uuencode data lines (excluding the count-0 terminator line).
    let line_count = if encoded {
        body_lines.len().saturating_sub(1) as u32
    } else {
        body_lines.len() as u32
    };
    let stats = DeltaStats::new(line_count, 0, 0);

    // Create delta entry
    let delta = DeltaEntry {
        delta_type: DeltaType::Normal,
        sid,
        datetime: now,
        user,
        serial: 1,
        pred_serial: 0,
        stats,
        included: Vec::new(),
        excluded: Vec::new(),
        ignored: Vec::new(),
        mr_numbers,
        comments: vec![comment_text],
    };

    // Create body with insert block
    let mut body = Vec::new();
    body.push(BodyRecord::Insert(1));
    for line in body_lines {
        body.push(BodyRecord::Text(line));
    }
    body.push(BodyRecord::End(1));

    // Create SCCS file
    let sccs = SccsFile {
        header: SccsHeader {
            checksum: 0, // Will be computed during serialization
            deltas: vec![delta],
            users,
            flags,
            descriptive_text: desc_text,
        },
        body,
    };

    // Atomic write: write to x-file, then rename
    let x_file = paths::xfile_from_sfile(path);
    let serialized = sccs.to_bytes();

    // SCCS s-files are read-only (r--r--r--).
    write_xfile_atomic(
        path,
        &x_file,
        &serialized,
        fs::Permissions::from_mode(0o444),
    )?;

    Ok(())
}

fn check_sccs_file(path: &Path) -> io::Result<bool> {
    let data = fs::read(path)?;

    // Parse to verify structure
    match SccsFile::from_bytes(&data) {
        Ok(sccs) => {
            // Verify checksum
            // Find end of first line (checksum line)
            let newline_pos = data.iter().position(|&b| b == b'\n').unwrap_or(data.len());
            let content_start = newline_pos + 1;

            if content_start < data.len() {
                let computed = plib::sccsfile::compute_checksum(&data[content_start..]);
                if computed != sccs.header.checksum {
                    eprintln!(
                        "{}: {} {}, {} {}",
                        path.display(),
                        gettext("checksum error: stored"),
                        sccs.header.checksum,
                        gettext("computed"),
                        computed
                    );
                    return Ok(false);
                }
            }
            Ok(true)
        }
        Err(e) => {
            eprintln!("{}: {}", path.display(), e);
            Ok(false)
        }
    }
}

fn recompute_checksum(path: &Path) -> io::Result<()> {
    // Per-command z-file lock around the rewrite.
    let _zlock = acquire_zlock(path)?;

    // Read and parse
    let sccs = SccsFile::from_path(path)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // Re-serialize (this recomputes checksum)
    let x_file = paths::xfile_from_sfile(path);
    let serialized = sccs.to_bytes();

    // Get original permissions
    let original_perms = fs::metadata(path)?.permissions();

    write_xfile_atomic(path, &x_file, &serialized, original_perms)?;

    Ok(())
}

fn modify_existing_file(
    path: &Path,
    desc_text: Option<Vec<String>>,
    add_flags: Vec<SccsFlag>,
    delete_flags: Vec<char>,
    add_users: Vec<String>,
    remove_users: Vec<String>,
) -> io::Result<()> {
    // Per-command z-file lock around the modify.
    let _zlock = acquire_zlock(path)?;

    // Read existing file
    let mut sccs = SccsFile::from_path(path)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // Update descriptive text if provided
    if let Some(text) = desc_text {
        sccs.header.descriptive_text = text;
    }

    // Delete flags
    for flag_char in delete_flags {
        sccs.header.flags.retain(|f| f.flag_char() != flag_char);
    }

    // Add flags
    for flag in add_flags {
        // Remove existing flag of same type first
        let fc = flag.flag_char();
        sccs.header.flags.retain(|f| f.flag_char() != fc);
        sccs.header.flags.push(flag);
    }

    // Add users
    for user in add_users {
        if !sccs.header.users.contains(&user) {
            sccs.header.users.push(user);
        }
    }

    // Remove users
    for user in remove_users {
        sccs.header.users.retain(|u| u != &user);
    }

    // Write atomically
    let x_file = paths::xfile_from_sfile(path);
    let original_perms = fs::metadata(path)?.permissions();

    let serialized = sccs.to_bytes();
    write_xfile_atomic(path, &x_file, &serialized, original_perms)?;

    Ok(())
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    plib::sccsfile::install_sigint_cleanup();

    // Pre-process argv to strip the attached-only options (-i/-t/-y) before
    // clap parses; clap cannot express "attached value only".
    let (attached, rest) = extract_attached_opts(std::env::args());
    let args = match Args::try_parse_from(&rest) {
        Ok(a) => a,
        Err(e) => {
            e.print().ok();
            return ExitCode::FAILURE;
        }
    };

    // Handle --help manually since we disabled automatic help
    if args.help {
        use clap::CommandFactory;
        Args::command().print_help().ok();
        println!();
        return ExitCode::SUCCESS;
    }

    // -i implies -n.
    let creating_new = args.new_file || attached.init_file.is_some();

    // Parse flags to add
    let mut add_flags = Vec::new();
    for flag_str in &args.set_flags {
        match parse_flag(flag_str) {
            Ok(flag) => add_flags.push(flag),
            Err(e) => {
                eprintln!("admin: {}", e);
                return ExitCode::FAILURE;
            }
        }
    }

    // Determine whether the 'v' (MR validation) flag is being set, for -m
    // consistency checks on create.
    let setting_v_flag = add_flags
        .iter()
        .any(|f| matches!(f, SccsFlag::MrValidation(_)));

    // Parse -m MR numbers (whitespace-separated list).
    let mr_numbers: Vec<String> = attached_mr_list(args.mrlist.as_deref());

    // -m requires the v flag (spec: "the application shall ensure that the v
    // flag is set"). Diagnose using cssc-compatible wording.
    if !mr_numbers.is_empty() && !setting_v_flag {
        eprintln!(
            "admin: {}",
            gettext("MRs not enabled with 'v' flag, can't use 'm' keyword.")
        );
        return ExitCode::FAILURE;
    }

    // Parse flags to delete
    let delete_flags: Vec<char> = args
        .delete_flags
        .iter()
        .filter_map(|s| s.chars().next())
        .collect();

    let mut exit_code = ExitCode::SUCCESS;

    // Expand operands: a lone `-` reads s-file names from stdin; a directory
    // operand expands to its sorted s.* members. The -i stdin-content path is
    // separate (operands here are never `-` when creating from stdin).
    let files = paths::expand_operands(&args.files);

    for file_path in &files {
        // Validate file name starts with s.
        if !paths::is_sfile(file_path) {
            eprintln!(
                "admin: {}: {}",
                file_path.display(),
                gettext("SCCS file names must begin with 's.'")
            );
            exit_code = ExitCode::FAILURE;
            continue;
        }

        if args.check {
            // -h: check file structure and checksum
            match check_sccs_file(file_path) {
                Ok(true) => {}
                Ok(false) => exit_code = ExitCode::FAILURE,
                Err(e) => {
                    eprintln!("admin: {}: {}", file_path.display(), e);
                    exit_code = ExitCode::FAILURE;
                }
            }
        } else if args.recompute_checksum {
            // -z: recompute checksum
            if let Err(e) = recompute_checksum(file_path) {
                eprintln!("admin: {}: {}", file_path.display(), e);
                exit_code = ExitCode::FAILURE;
            }
        } else if creating_new {
            // Creating a new SCCS file
            if file_path.exists() {
                eprintln!(
                    "admin: {}: {}",
                    file_path.display(),
                    gettext("file already exists")
                );
                exit_code = ExitCode::FAILURE;
                continue;
            }

            // Read input content. init_file: None => -n only (empty body);
            // Some(None) => bare -i (read stdin); Some(Some(p)) => -ip (file,
            // or stdin if p is empty).
            let content = match &attached.init_file {
                Some(Some(path)) if !path.is_empty() => read_input_file(Some(path)),
                Some(_) => read_input_file(None),
                None => Ok(Vec::new()),
            };

            let content = match content {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("admin: {}", e);
                    exit_code = ExitCode::FAILURE;
                    continue;
                }
            };

            // Read descriptive text if -t specified with a name.
            let desc_text = match &attached.desc_text {
                Some(Some(path)) if !path.is_empty() => read_desc_text(Some(path)),
                _ => Ok(Vec::new()),
            };

            let desc_text = match desc_text {
                Ok(t) => t,
                Err(e) => {
                    eprintln!("admin: {}", e);
                    exit_code = ExitCode::FAILURE;
                    continue;
                }
            };

            // v flag set on create requires MR numbers via -m.
            if setting_v_flag && mr_numbers.is_empty() {
                eprintln!(
                    "admin: {}: {}",
                    file_path.display(),
                    gettext("MR number(s) must be supplied.")
                );
                exit_code = ExitCode::FAILURE;
                continue;
            }

            // Get comment: bare -y or -ytext both supply an explicit comment
            // (empty for bare -y); absence yields the default comment.
            let comment: Option<&str> = match &attached.comment {
                Some(Some(c)) => Some(c.as_str()),
                Some(None) => Some(""),
                None => None,
            };

            if let Err(e) = create_new_sccs_file(
                file_path,
                NewFileParams {
                    content,
                    initial_sid: args.initial_sid.as_deref(),
                    comment,
                    desc_text,
                    flags: add_flags.clone(),
                    users: args.add_users.clone(),
                    mr_numbers: mr_numbers.clone(),
                },
            ) {
                eprintln!("admin: {}: {}", file_path.display(), e);
                exit_code = ExitCode::FAILURE;
            }
        } else {
            // Modifying existing file
            if !file_path.exists() {
                eprintln!(
                    "admin: {}: {}",
                    file_path.display(),
                    gettext("file does not exist")
                );
                exit_code = ExitCode::FAILURE;
                continue;
            }

            // Get descriptive text if -t specified. -t name replaces; bare -t
            // (or -t with empty value) removes existing descriptive text.
            let desc_text = match &attached.desc_text {
                Some(Some(path)) if !path.is_empty() => Some(read_desc_text(Some(path))),
                Some(_) => Some(Ok(Vec::new())), // remove
                None => None,
            };

            let desc_text = match desc_text {
                Some(Ok(t)) => Some(t),
                Some(Err(e)) => {
                    eprintln!("admin: {}", e);
                    exit_code = ExitCode::FAILURE;
                    continue;
                }
                None => None,
            };

            if let Err(e) = modify_existing_file(
                file_path,
                desc_text,
                add_flags.clone(),
                delete_flags.clone(),
                args.add_users.clone(),
                args.erase_users.clone(),
            ) {
                eprintln!("admin: {}: {}", file_path.display(), e);
                exit_code = ExitCode::FAILURE;
            }
        }
    }

    exit_code
}
