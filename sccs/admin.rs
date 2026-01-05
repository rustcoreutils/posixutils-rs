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
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use plib::sccsfile::{
    BodyRecord, DeltaEntry, DeltaStats, DeltaType, SccsDateTime, SccsFile, SccsFlag, SccsHeader,
    Sid, paths,
};

/// admin - create and administer SCCS files
#[derive(Parser)]
#[command(version, about = gettext("admin - create and administer SCCS files"), disable_help_flag = true)]
struct Args {
    #[arg(long = "help", help = gettext("Print help information"))]
    help: bool,

    #[arg(short = 'n', help = gettext("Create a new SCCS file"))]
    new_file: bool,

    #[arg(short = 'i', value_name = "NAME", num_args = 0..=1, default_missing_value = "", help = gettext("Initialize from file (use stdin if no name given)"))]
    init_file: Option<String>,

    #[arg(short = 'r', value_name = "SID", help = gettext("Initial SID for new file"))]
    initial_sid: Option<String>,

    #[arg(short = 'y', value_name = "COMMENT", num_args = 0..=1, default_missing_value = "", help = gettext("Comment for initial delta"))]
    comment: Option<String>,

    #[arg(short = 't', value_name = "NAME", num_args = 0..=1, default_missing_value = "", help = gettext("Descriptive text file"))]
    desc_text: Option<String>,

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

fn get_username() -> String {
    std::env::var("USER")
        .or_else(|_| std::env::var("LOGNAME"))
        .unwrap_or_else(|_| "unknown".to_string())
}

fn read_input_file(path: Option<&str>) -> io::Result<Vec<String>> {
    let reader: Box<dyn BufRead> = match path {
        Some(p) => Box::new(BufReader::new(File::open(p)?)),
        None => Box::new(BufReader::new(io::stdin())),
    };

    let mut lines = Vec::new();
    for line in reader.lines() {
        lines.push(line?);
    }
    Ok(lines)
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

fn parse_flag(flag_str: &str) -> Result<SccsFlag, String> {
    if flag_str.is_empty() {
        return Err("Empty flag".to_string());
    }

    let flag_char = flag_str.chars().next().unwrap();
    let value = if flag_str.len() > 1 {
        &flag_str[1..]
    } else {
        ""
    };

    SccsFlag::parse(flag_char, value).map_err(|e| e.to_string())
}

fn create_new_sccs_file(
    path: &Path,
    content_lines: Vec<String>,
    initial_sid: Option<&str>,
    comment: Option<&str>,
    desc_text: Vec<String>,
    flags: Vec<SccsFlag>,
    users: Vec<String>,
) -> io::Result<()> {
    // Determine initial SID
    let sid = match initial_sid {
        Some(s) => parse_sid(s).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?,
        None => Sid::trunk(1, 1),
    };

    // Validate: must be trunk SID
    if !sid.is_trunk() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Initial SID must be a trunk SID (no branch component)",
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

    // Calculate stats
    let line_count = content_lines.len() as u32;
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
        mr_numbers: Vec::new(),
        comments: vec![comment_text],
    };

    // Create body with insert block
    let mut body = Vec::new();
    body.push(BodyRecord::Insert(1));
    for line in content_lines {
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

    fs::write(&x_file, &serialized)?;

    // Set read-only permissions
    let perms = fs::Permissions::from_mode(0o444);
    fs::set_permissions(&x_file, perms)?;

    // Rename x-file to s-file
    fs::rename(&x_file, path)?;

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
                        "{}: checksum error: stored {}, computed {}",
                        path.display(),
                        sccs.header.checksum,
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
    // Read and parse
    let sccs = SccsFile::from_path(path)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // Re-serialize (this recomputes checksum)
    let x_file = paths::xfile_from_sfile(path);
    let serialized = sccs.to_bytes();

    // Get original permissions
    let original_perms = fs::metadata(path)?.permissions();

    fs::write(&x_file, &serialized)?;
    fs::set_permissions(&x_file, original_perms.clone())?;
    fs::rename(&x_file, path)?;

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
    fs::write(&x_file, &serialized)?;
    fs::set_permissions(&x_file, original_perms)?;
    fs::rename(&x_file, path)?;

    Ok(())
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();

    // Handle --help manually since we disabled automatic help
    if args.help {
        use clap::CommandFactory;
        Args::command().print_help().ok();
        println!();
        return ExitCode::SUCCESS;
    }

    // -i implies -n (init_file is Some("") for stdin, Some(path) for file)
    let creating_new = args.new_file || args.init_file.is_some();

    // Parse flags to add
    let mut add_flags = Vec::new();
    for flag_str in &args.set_flags {
        match parse_flag(flag_str) {
            Ok(flag) => add_flags.push(flag),
            Err(e) => {
                eprintln!("admin: {}: {}", gettext("invalid flag"), e);
                return ExitCode::FAILURE;
            }
        }
    }

    // Parse flags to delete
    let delete_flags: Vec<char> = args
        .delete_flags
        .iter()
        .filter_map(|s| s.chars().next())
        .collect();

    let mut exit_code = ExitCode::SUCCESS;

    for file_path in &args.files {
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

            // Read input content
            let content = match &args.init_file {
                Some(path) if !path.is_empty() => read_input_file(Some(path)),
                Some(_) => read_input_file(None), // Empty string means stdin
                None => Ok(Vec::new()),           // Empty file with -n only
            };

            let content = match content {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("admin: {}", e);
                    exit_code = ExitCode::FAILURE;
                    continue;
                }
            };

            // Read descriptive text if specified
            let desc_text = match &args.desc_text {
                Some(path) if !path.is_empty() => read_desc_text(Some(path)),
                Some(_) => Ok(Vec::new()), // Empty string means clear
                None => Ok(Vec::new()),
            };

            let desc_text = match desc_text {
                Ok(t) => t,
                Err(e) => {
                    eprintln!("admin: {}", e);
                    exit_code = ExitCode::FAILURE;
                    continue;
                }
            };

            // Get comment
            let comment: Option<&str> = args.comment.as_deref();

            if let Err(e) = create_new_sccs_file(
                file_path,
                content,
                args.initial_sid.as_deref(),
                comment,
                desc_text,
                add_flags.clone(),
                args.add_users.clone(),
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

            // Get descriptive text if -t specified
            let desc_text = match &args.desc_text {
                Some(path) if !path.is_empty() => Some(read_desc_text(Some(path))),
                Some(_) => Some(Ok(Vec::new())), // Empty string means clear
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
