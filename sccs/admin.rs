//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! admin - create and administer SCCS files

use std::fs;
use std::io::{self, Read};
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::gettext;
use plib::sccsfile::{
    paths, DeltaEntry, DeltaStats, DeltaType, NewFile, ReleaseLock, SccsDateTime, SccsFile,
    SccsFlag, Sid,
};
use posixutils_sccs::{diag, idkw, mrlist, sfio, zlock};

/// admin - create and administer SCCS files
#[derive(Parser)]
#[command(version, about = gettext("admin - create and administer SCCS files"), disable_help_flag = true)]
struct Args {
    #[arg(long = "help", help = gettext("Print help information"))]
    help: bool,

    #[arg(short = 'n', help = gettext("Create a new SCCS file"))]
    new_file: bool,

    #[arg(short = 'r', value_name = "SID", allow_hyphen_values = true, help = gettext("Initial SID for new file"))]
    initial_sid: Option<String>,

    #[arg(short = 'm', value_name = "MRLIST", allow_hyphen_values = true, help = gettext("MR numbers for initial delta"))]
    mrlist: Option<String>,

    #[arg(short = 'f', value_name = "FLAG", allow_hyphen_values = true, help = gettext("Set flag (can be repeated)"))]
    set_flags: Vec<String>,

    #[arg(short = 'd', value_name = "FLAG", allow_hyphen_values = true, help = gettext("Delete flag (can be repeated)"))]
    delete_flags: Vec<String>,

    #[arg(short = 'a', value_name = "LOGIN", allow_hyphen_values = true, help = gettext("Add user to authorized list"))]
    add_users: Vec<String>,

    #[arg(short = 'e', value_name = "LOGIN", allow_hyphen_values = true, help = gettext("Remove user from authorized list"))]
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

/// Option letters that take an option-argument, attached or as the next word.
const ARGUMENT_LETTERS: &str = "rmfdae";

/// Split argv into the attached-only options (`-i`/`-t`/`-y`) and the remaining
/// words, for clap to parse.
///
/// The words are read as the Utility Syntax Guidelines read them (XBD 12.2),
/// so that only an option is taken for one:
/// - in a group of letters (`-niin.txt`), `-i`, `-t` or `-y` takes the rest
///   of the word as its argument, and the flags before it stay for clap;
/// - a letter that takes an argument ends its group, and the argument --
///   the rest of the word, or the whole next word -- is passed over unread,
///   so `-m -yMR` is an MR and not a comment;
/// - after `--` every word is an operand.
///
/// Matching every word that begins `-i`, `-t` or `-y`, as this did, took
/// option-arguments and operands for options, and missed the grouped forms.
fn extract_attached_opts<I>(argv: I) -> (AttachedOpts, Vec<String>)
where
    I: IntoIterator<Item = String>,
{
    let mut opts = AttachedOpts::default();
    let mut rest = Vec::new();
    let mut words = argv.into_iter();

    // argv[0] is the utility's name.
    rest.extend(words.next());

    while let Some(word) = words.next() {
        if word == "--" {
            rest.push(word);
            rest.extend(words);
            break;
        }
        // An operand, a lone "-", or a long option: not a group of letters.
        if !word.starts_with('-') || word == "-" || word.starts_with("--") {
            rest.push(word);
            continue;
        }

        // What becomes of the word: it goes to clap whole, unless -i, -t or
        // -y ends it, which leaves clap only the flags grouped before it.
        let mut for_clap = Some(word.as_str());
        let mut argument_word = None;
        for (at, letter) in word.char_indices().skip(1) {
            let after = at + letter.len_utf8();
            if ARGUMENT_LETTERS.contains(letter) {
                // Its argument is the rest of the word, or the next word,
                // which is passed over unread.
                if after == word.len() {
                    argument_word = words.next();
                }
                break;
            }
            let field = match letter {
                'i' => &mut opts.init_file,
                't' => &mut opts.desc_text,
                'y' => &mut opts.comment,
                _ => continue, // a flag, or a letter for clap to refuse
            };
            let value = &word[after..];
            *field = Some((!value.is_empty()).then(|| value.to_string()));
            for_clap = (at > 1).then(|| &word[..at]);
            break;
        }
        rest.extend(for_clap.map(str::to_string));
        rest.extend(argument_word);
    }

    (opts, rest)
}

/// The text for a new file: the `-i` file `path`, or standard input. An
/// error comes back as the diagnostic's text, naming what could not be read.
fn read_input_file(path: Option<&str>) -> Result<Vec<u8>, String> {
    let read = match path {
        Some(p) => fs::read(p),
        None => {
            let mut data = Vec::new();
            io::stdin().lock().read_to_end(&mut data).map(|_| data)
        }
    };
    read.map_err(|e| {
        format!(
            "{}: {}",
            path.unwrap_or("standard input"),
            plib::diag::io_error_text(&e)
        )
    })
}

/// The descriptive text in the `-t` file `path`, as its lines. An error comes
/// back as the diagnostic's text, naming the file.
fn read_desc_text(path: &str) -> Result<Vec<Vec<u8>>, String> {
    let data =
        fs::read(path).map_err(|e| format!("{}: {}", path, plib::diag::io_error_text(&e)))?;
    let lines = plib::sccsfile::split_lines(&data);
    // The text sits between ^At and ^AT: a line of it that began with SOH
    // would read as a control line and end it early.
    if let Some(n) = lines.iter().position(|l| l.first() == Some(&0x01)) {
        return Err(format!(
            "{}: {} {}: {}",
            path,
            gettext("line"),
            n + 1,
            gettext("descriptive text cannot begin with a control character")
        ));
    }
    Ok(lines.into_iter().map(<[u8]>::to_vec).collect())
}

fn parse_sid(s: &str) -> Result<Sid, String> {
    s.parse::<Sid>().map_err(|e| e.to_string())
}

/// The highest release number.
const MAX_RELEASE: u16 = 9999;

/// Flag letters recognized by the `-f` option (POSIX admin 84028-84079).
///
/// `e`, the encoded-body marker, is not one of them: admin sets it when it
/// stores a body encoded, and a body is read by it. Set or removed by hand it
/// would say one thing while the body is stored the other way.
const VALID_FLAG_CHARS: &[char] = &['b', 'c', 'd', 'f', 'i', 'j', 'l', 'm', 'n', 'q', 't', 'v'];

/// The diagnostic for a letter that is not a flag.
fn unrecognized_flag(letter: char) -> String {
    format!("{} '{}'", gettext("Unrecognized flag"), letter)
}

fn parse_flag(flag_str: &str) -> Result<SccsFlag, String> {
    if flag_str.is_empty() {
        return Err(gettext("Empty flag"));
    }
    // A flag is one line of the s-file; a value holding a newline would put
    // a line of its own into the header.
    if flag_str.contains('\n') {
        return Err(gettext("A flag value cannot contain a newline"));
    }

    let flag_char = flag_str.chars().next().unwrap();
    let value = &flag_str[flag_char.len_utf8()..];

    // Reject any flag letter not in the spec set. SccsFlag::parse maps unknown
    // letters to SccsFlag::Unknown for lossless reads of existing files, but
    // admin must not write a malformed flag from a bad -f argument.
    if !VALID_FLAG_CHARS.contains(&flag_char) {
        return Err(unrecognized_flag(flag_char));
    }

    let flag = SccsFlag::parse(flag_char, value.as_bytes()).map_err(|e| e.to_string())?;

    // The ceiling is "a number less than or equal to 9 999" and the floor
    // "a number greater than 0 but less than 9 999" (POSIX 84030-84034).
    let out_of_range = |n: u16, range: &str| {
        format!(
            "{} '{}' {} ({})",
            gettext("value"),
            n,
            gettext("out of range"),
            range
        )
    };
    match &flag {
        SccsFlag::Ceiling(n) if *n > MAX_RELEASE => {
            return Err(out_of_range(*n, "0-9999"));
        }
        SccsFlag::Floor(n) if !(1..MAX_RELEASE).contains(n) => {
            return Err(out_of_range(*n, "1-9998"));
        }
        _ => {}
    }

    Ok(flag)
}

/// The login a `-a`/`-e` argument names, as it is stored: one line of the
/// user list, without the blanks around it, which reading the list back
/// drops -- kept, a login added as " bob " could never be matched again. It
/// cannot be empty, hold a newline, or begin with the SOH that marks a
/// control line (`^AU` ends the list).
fn login_of(arg: &str) -> Result<String, String> {
    let login = arg.trim_ascii();
    if login.is_empty() || login.contains('\n') || login.starts_with('\x01') {
        return Err(format!(
            "'{}': {}",
            arg.escape_default(),
            gettext("invalid login name")
        ));
    }
    Ok(login.to_string())
}

/// What one `-d` option removes.
#[derive(Clone)]
enum FlagRemoval {
    /// The flag with this letter, whatever its value. The letter may be one
    /// outside the POSIX set, for a flag another implementation left in the
    /// file ([`remove_flag`] refuses it if the file has no such flag).
    Flag(char),
    /// These releases from the `l` flag's list of locked releases: "the l
    /// list flag gives a list of releases to be unlocked" (POSIX 84082-84083).
    Unlock(Vec<u16>),
}

/// Parse a `-d` option-argument: a flag letter `-f` takes, and for `l` an
/// optional list of releases to unlock (`a`, or none, unlocks them all).
fn parse_flag_removal(arg: &str) -> Result<FlagRemoval, String> {
    let Some(letter) = arg.chars().next() else {
        return Err(gettext("Empty flag"));
    };
    if letter == 'e' {
        return Err(gettext(
            "The 'e' flag records how the text is stored and cannot be removed",
        ));
    }
    let list = arg[letter.len_utf8()..].trim();
    if letter != 'l' || list.is_empty() || list == "a" {
        return Ok(FlagRemoval::Flag(letter));
    }
    list.split(',')
        .map(|rel| rel.trim().parse::<u16>())
        .collect::<Result<Vec<u16>, _>>()
        .map(FlagRemoval::Unlock)
        .map_err(|_| format!("{} '{}'", gettext("Invalid release list"), list))
}

/// Apply one `-d` option to an s-file.
fn remove_flag(sccs: &mut SccsFile, removal: FlagRemoval) -> io::Result<()> {
    let invalid = |message: String| io::Error::new(io::ErrorKind::InvalidInput, message);
    let releases = match removal {
        FlagRemoval::Flag(letter) => {
            // A letter that is not a flag is only removable if the file has
            // it, as a flag some other implementation wrote.
            if !sccs.remove_flag(letter) && !VALID_FLAG_CHARS.contains(&letter) {
                return Err(invalid(unrecognized_flag(letter)));
            }
            return Ok(());
        }
        FlagRemoval::Unlock(releases) => releases,
    };
    match sccs.locked_releases() {
        None => Ok(()), // nothing is locked
        // "Every release" is not a list a release can be taken out of.
        Some(ReleaseLock::All) => Err(invalid(gettext(
            "every release is locked ('l a'); use -dla to unlock them all",
        ))),
        Some(ReleaseLock::List(locked)) => {
            let rest: Vec<u16> = locked
                .iter()
                .copied()
                .filter(|rel| !releases.contains(rel))
                .collect();
            if rest.is_empty() {
                sccs.remove_flag('l');
            } else {
                sccs.set_flag(SccsFlag::LockedReleases(ReleaseLock::List(rest)));
            }
            Ok(())
        }
    }
}

/// The diagnostic for `-m` without the `v` flag, in CSSC's wording.
fn mrs_need_v_flag() -> String {
    gettext("MRs not enabled with 'v' flag, can't use 'm' keyword.")
}

/// Inputs for creating a new SCCS file.
struct NewFileParams<'a> {
    content: Vec<u8>,
    initial_sid: Option<&'a str>,
    comment: Option<&'a str>,
    desc_text: Vec<Vec<u8>>,
    flags: Vec<SccsFlag>,
    users: Vec<String>,
    mr_numbers: Vec<String>,
}

/// Whether anything is at `path`: a file, or a symlink even if it dangles.
fn name_is_taken(path: &Path) -> bool {
    fs::symlink_metadata(path).is_ok()
}

fn create_new_sccs_file(path: &Path, params: NewFileParams) -> io::Result<()> {
    // Per-command z-file lock around the create.
    let _zlock = zlock::acquire(path)?;

    // The check that counts is this one, made holding the lock: whoever
    // creates the file holds it too, so the name cannot be taken between
    // here and the rename. Checked only before the lock, of two creations
    // racing the second replaced the first's file.
    if name_is_taken(path) {
        return Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            gettext("file already exists"),
        ));
    }

    let NewFileParams {
        content,
        initial_sid,
        comment,
        desc_text,
        mut flags,
        users,
        mr_numbers,
    } = params;

    // Determine initial SID. "The level number is optional, and defaults to 1"
    // (POSIX 84017-84019): `-r2` names release 2, not the level-0 SID `2.0`,
    // which is not a valid delta number at all and would poison every later
    // get and delta on the file.
    let sid = match initial_sid {
        Some(s) => {
            let mut sid =
                parse_sid(s).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;
            if sid.lev == 0 {
                sid.lev = 1;
            }
            // Releases run from 1: a release-0 delta lies below any floor,
            // so the file could be created but never edited.
            if !(1..=MAX_RELEASE).contains(&sid.rel) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    gettext("Initial release must be from 1 to 9999"),
                ));
            }
            sid
        }
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
    let user = posixutils_sccs::username();

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
    // What a text s-file cannot hold -- a NUL, a line beginning with SOH, a
    // last line without its newline -- is stored encoded.
    let encoded = plib::sccsfile::not_storable_as_text(&content).is_some();

    // Warn (non-fatal) when a text body has no SCCS id keyword, matching cssc.
    if !encoded && !idkw::contains(&content) {
        eprintln!(
            "admin: {}: {}: {}",
            gettext("warning"),
            path.display(),
            gettext("No id keywords.")
        );
    }

    let body_lines: Vec<Vec<u8>> = if encoded {
        flags.push(SccsFlag::Encoded(1));
        plib::sccsfile::uuencode_sccs(&content)
    } else {
        // Plain text: its lines, each ending at a newline. A CR before the
        // newline is part of the line.
        plib::sccsfile::split_lines(&content)
            .into_iter()
            .map(<[u8]>::to_vec)
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
        user: user.into_bytes(),
        serial: 1,
        pred_serial: 0,
        stats,
        included: Vec::new(),
        excluded: Vec::new(),
        ignored: Vec::new(),
        mr_numbers: mr_numbers.into_iter().map(String::into_bytes).collect(),
        comments: posixutils_sccs::comment_lines(&comment_text),
    };

    let sccs = SccsFile::create(NewFile {
        delta,
        users: &users
            .into_iter()
            .map(String::into_bytes)
            .collect::<Vec<_>>(),
        flags: &flags,
        descriptive_text: &desc_text,
        lines: &body_lines,
    })
    .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e.to_string()))?;

    // Atomic write: write to x-file, then rename
    let x_file = paths::xfile_from_sfile(path);
    let serialized = sccs.to_bytes();

    // SCCS s-files are read-only (r--r--r--).
    sfio::write_xfile_atomic(
        path,
        &x_file,
        &serialized,
        fs::Permissions::from_mode(0o444),
    )?;

    Ok(())
}

fn check_sccs_file(path: &Path) -> io::Result<bool> {
    let data = fs::read(path)?;

    // Parse strictly to verify the structure
    match SccsFile::from_bytes_strict(&data) {
        Ok(sccs) => {
            // Verify the checksum. A header-only s-file has an empty body,
            // which still has a checksum: skipping the check for it let
            // `admin -h` pass a file `val` rejected.
            if !plib::sccsfile::stored_checksum_matches(&data, sccs.header().checksum) {
                let computed = plib::sccsfile::stored_body_checksum(&data);
                diag::error_path(
                    "admin",
                    path,
                    &format!(
                        "{} {}, {} {}",
                        gettext("checksum error: stored"),
                        sccs.header().checksum,
                        gettext("computed"),
                        computed
                    ),
                );
                return Ok(false);
            }
            Ok(true)
        }
        Err(e) => {
            diag::error_path("admin", path, &e.to_string());
            Ok(false)
        }
    }
}

fn recompute_checksum(path: &Path) -> io::Result<()> {
    // Per-command z-file lock around the rewrite.
    let _zlock = zlock::acquire(path)?;

    // It must be an s-file, but "use of this option on a truly corrupted file
    // may prevent future detection of the corruption" (POSIX 84112-84113):
    // only the checksum line changes. Re-serializing the parsed file instead
    // silently deleted every line the parser reads around.
    let data = fs::read(path)?;
    SccsFile::from_bytes(&data)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;
    let x_file = paths::xfile_from_sfile(path);
    let serialized = plib::sccsfile::with_recomputed_checksum(&data);

    // Get original permissions
    let original_perms = fs::metadata(path)?.permissions();

    sfio::write_xfile_atomic(path, &x_file, &serialized, original_perms)?;

    Ok(())
}

fn modify_existing_file(
    path: &Path,
    desc_text: Option<Vec<Vec<u8>>>,
    add_flags: Vec<SccsFlag>,
    delete_flags: Vec<FlagRemoval>,
    add_users: Vec<String>,
    remove_users: Vec<String>,
    mrs_given: bool,
) -> io::Result<()> {
    // Per-command z-file lock around the modify.
    let _zlock = zlock::acquire(path)?;

    // Read existing file
    let mut sccs = SccsFile::from_path(path)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // Update descriptive text if provided
    if let Some(text) = desc_text {
        sccs.set_descriptive_text(text);
    }

    // Delete flags
    for removal in delete_flags {
        remove_flag(&mut sccs, removal)?;
    }

    // Add flags
    for flag in add_flags {
        sccs.set_flag(flag);
    }

    // -m needs the v flag: the one this file ends up with, whether it had
    // it already or this command sets it. (There is no initial delta left
    // for the MRs to mark, so with the flag set they change nothing.)
    if mrs_given && sccs.mr_program().is_none() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            mrs_need_v_flag(),
        ));
    }

    // Add users
    for user in add_users {
        sccs.add_user(user.as_bytes());
    }

    // Remove users
    for user in remove_users {
        sccs.remove_user(user.as_bytes());
    }

    // Write atomically
    let x_file = paths::xfile_from_sfile(path);
    let original_perms = fs::metadata(path)?.permissions();

    let serialized = sccs.to_bytes();
    sfio::write_xfile_atomic(path, &x_file, &serialized, original_perms)?;

    Ok(())
}

fn main() -> ExitCode {
    plib::diag::init_locale("admin");

    zlock::install_cleanup();

    // Pre-process argv to strip the attached-only options (-i/-t/-y) before
    // clap parses; clap cannot express "attached value only".
    let (attached, rest) = extract_attached_opts(plib::optarg::args_utf8("admin"));
    let args = match Args::try_parse_from(plib::optarg::keep_leading_equals::<Args>(rest)) {
        Ok(a) => a,
        // The version is written to standard output with status 0.
        Err(e) if !e.use_stderr() => plib::optarg::exit(e),
        Err(e) => {
            e.print().ok();
            return ExitCode::FAILURE;
        }
    };

    // Handle --help manually since we disabled automatic help
    if args.help {
        use clap::CommandFactory;
        use std::io::Write;
        let written = Args::command().print_help().and_then(|()| {
            let mut stdout = std::io::stdout();
            writeln!(stdout)?;
            stdout.flush()
        });
        if let Err(e) = written {
            plib::diag::write_error(&e);
            return ExitCode::FAILURE;
        }
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

    let logins = |args: &[String]| args.iter().map(|a| login_of(a)).collect();
    let (add_users, erase_users): (Vec<String>, Vec<String>) =
        match (logins(&args.add_users), logins(&args.erase_users)) {
            (Ok(add), Ok(erase)) => (add, erase),
            (Err(e), _) | (_, Err(e)) => {
                eprintln!("admin: {}", e);
                return ExitCode::FAILURE;
            }
        };

    // The 'v' (MR validation) flag this command line sets, if any: the outer
    // Option is "being set", the inner one its validation program.
    let v_flag_set: Option<Option<&[u8]>> = add_flags.iter().find_map(|f| match f {
        SccsFlag::MrValidation(program) => Some(program.as_deref()),
        _ => None,
    });

    // Parse -m MR numbers (whitespace-separated list).
    let mr_numbers: Vec<String> = mrlist::parse(args.mrlist.as_deref());

    // Parse flags to delete
    let mut delete_flags = Vec::new();
    for arg in &args.delete_flags {
        match parse_flag_removal(arg) {
            Ok(removal) => delete_flags.push(removal),
            Err(e) => {
                eprintln!("admin: {}", e);
                return ExitCode::FAILURE;
            }
        }
    }
    // A new file has no flag from elsewhere to remove, so there the letter
    // must be a flag's.
    if creating_new {
        let stranger = delete_flags.iter().find_map(|removal| match removal {
            FlagRemoval::Flag(letter) if !VALID_FLAG_CHARS.contains(letter) => Some(*letter),
            _ => None,
        });
        if let Some(letter) = stranger {
            eprintln!("admin: {}", unrecognized_flag(letter));
            return ExitCode::FAILURE;
        }
    }

    let mut exit_code = ExitCode::SUCCESS;

    // Expand operands: a lone `-` reads s-file names from stdin; a directory
    // operand expands to its sorted s.* members. The -i stdin-content path is
    // separate (operands here are never `-` when creating from stdin).
    let files = paths::expand_operands(&args.files);

    // The message for a name that is not an s-file's.
    let not_an_sfile = |path: &Path| {
        diag::error_path(
            "admin",
            path,
            &gettext("SCCS file names must begin with 's.'"),
        )
    };

    // -i gives the text of one new file: with more, every file after the
    // first would be made from an input already read to its end. A detached
    // option-argument (`-i name s.file`) lands here too, so the names that
    // are not s-files are diagnosed first.
    if attached.init_file.is_some() && files.len() > 1 {
        files
            .iter()
            .filter(|f| !paths::is_sfile(f))
            .for_each(|f| not_an_sfile(f));
        eprintln!(
            "admin: {}",
            gettext("the -i option cannot be used with more than one file")
        );
        return ExitCode::FAILURE;
    }

    for file_path in &files {
        // Validate file name starts with s.
        if !paths::is_sfile(file_path) {
            not_an_sfile(file_path);
            exit_code = ExitCode::FAILURE;
            continue;
        }

        if args.check {
            // -h: check file structure and checksum
            match check_sccs_file(file_path) {
                Ok(true) => {}
                Ok(false) => exit_code = ExitCode::FAILURE,
                Err(e) => {
                    eprintln!(
                        "admin: {}: {}",
                        file_path.display(),
                        plib::diag::io_error_text(&e)
                    );
                    exit_code = ExitCode::FAILURE;
                }
            }
        } else if args.recompute_checksum {
            // -z: recompute checksum
            if let Err(e) = recompute_checksum(file_path) {
                eprintln!(
                    "admin: {}: {}",
                    file_path.display(),
                    plib::diag::io_error_text(&e)
                );
                exit_code = ExitCode::FAILURE;
            }
        } else if creating_new {
            // Creating a new SCCS file. An early look, to refuse before the
            // input is read; create_new_sccs_file looks again under the lock.
            if name_is_taken(file_path) {
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
                Some(Some(path)) if !path.is_empty() => read_desc_text(path),
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

            // -m and the v flag go together on create: "the application
            // shall ensure that the v flag is set" for -m, and with v "the m
            // option is also used even if its value is null" (POSIX
            // 84077-84079, 84102-84106). MRs that are given are validated by
            // the flag's program, if it names one.
            let mr_problem = match (v_flag_set, &args.mrlist) {
                (None, Some(_)) => Some(mrs_need_v_flag()),
                (Some(_), None) => Some(gettext("MR number(s) must be supplied.")),
                _ => None,
            };
            if let Some(problem) = mr_problem {
                diag::error_path("admin", file_path, &problem);
                exit_code = ExitCode::FAILURE;
                continue;
            }
            if let Some(Some(program)) = v_flag_set {
                if !mr_numbers.is_empty()
                    && !mrlist::validate("admin", file_path, program, &mr_numbers)
                {
                    exit_code = ExitCode::FAILURE;
                    continue;
                }
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
                    users: add_users.clone(),
                    mr_numbers: mr_numbers.clone(),
                },
            ) {
                eprintln!(
                    "admin: {}: {}",
                    file_path.display(),
                    plib::diag::io_error_text(&e)
                );
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
                Some(Some(path)) if !path.is_empty() => Some(read_desc_text(path)),
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
                add_users.clone(),
                erase_users.clone(),
                args.mrlist.is_some(),
            ) {
                eprintln!(
                    "admin: {}: {}",
                    file_path.display(),
                    plib::diag::io_error_text(&e)
                );
                exit_code = ExitCode::FAILURE;
            }
        }
    }

    exit_code
}
