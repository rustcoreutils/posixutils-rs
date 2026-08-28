//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! sccs - front end for the SCCS subsystem

use std::env;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{self, Command, ExitCode};

use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::sccsfile::paths;

fn usage() -> ! {
    eprintln!(
        "{}",
        gettext("usage: sccs [-r] [-d path] [-p path] command [options...] [operands...]")
    );
    process::exit(2);
}

/// Partition `args` into options destined for the delta phase and options
/// destined for the get phase of a delget/deledit, plus the shared file
/// operands.
///
/// Routing is decided by the option *letter* (the character immediately after
/// the leading '-'), per the SCCS spec partitions, rather than by substring
/// matching of the whole token.  An option letter may legitimately route to
/// both phases (e.g. -s).  File operands (tokens not starting with '-') and
/// any unrecognized options are appended to both phases so the underlying
/// utility can diagnose them.
fn split_delget_opts<'a>(
    args: &[&'a str],
    delta_letters: &str,
    get_letters: &str,
) -> (Vec<&'a str>, Vec<&'a str>) {
    let mut delta_opts = Vec::new();
    let mut get_opts = Vec::new();

    for &a in args {
        if let Some(rest) = a.strip_prefix('-') {
            let letter = rest.chars().next();
            match letter {
                Some(c) => {
                    let to_delta = delta_letters.contains(c);
                    let to_get = get_letters.contains(c);
                    if to_delta {
                        delta_opts.push(a);
                    }
                    if to_get {
                        get_opts.push(a);
                    }
                    if !to_delta && !to_get {
                        // Unknown option: let both phases see it (and diagnose).
                        delta_opts.push(a);
                        get_opts.push(a);
                    }
                }
                None => {
                    // A bare "-" operand.
                    delta_opts.push(a);
                    get_opts.push(a);
                }
            }
        } else {
            // File operand: needed by both phases.
            delta_opts.push(a);
            get_opts.push(a);
        }
    }

    (delta_opts, get_opts)
}

/// Resolve a sibling SCCS utility relative to the directory of the running
/// executable.  Falls back to the bare command name (resolved via $PATH) when
/// the sibling cannot be located next to us.
fn sibling(cmd: &str) -> PathBuf {
    if let Ok(exe) = env::current_exe() {
        if let Some(dir) = exe.parent() {
            let candidate = dir.join(cmd);
            if candidate.exists() {
                return candidate;
            }
        }
    }
    PathBuf::from(cmd)
}

/// Look up a user's home directory from the passwd database (for the
/// `PROJECTDIR=<username>` form, where that user's home is examined for a
/// `src`/`source` directory).
fn user_home_dir(name: &str) -> Option<PathBuf> {
    use std::ffi::{CStr, CString};
    let cname = CString::new(name).ok()?;
    unsafe {
        let pw = libc::getpwnam(cname.as_ptr());
        if pw.is_null() || (*pw).pw_dir.is_null() {
            return None;
        }
        let dir = CStr::from_ptr((*pw).pw_dir).to_str().ok()?;
        Some(PathBuf::from(dir))
    }
}

/// Drop elevated privileges by resetting the effective user/group ids to the
/// real ones.  This is a no-op when sccs is not installed setuid/setgid, and
/// the correct behavior for `sccs -r` when it is.
fn drop_privileges() {
    unsafe {
        if libc::setgid(libc::getgid()) != 0 {
            eprintln!("sccs: setgid: {}", std::io::Error::last_os_error());
            process::exit(1);
        }
        if libc::setuid(libc::getuid()) != 0 {
            eprintln!("sccs: setuid: {}", std::io::Error::last_os_error());
            process::exit(1);
        }
    }
}

/// Convert a file operand to SCCS file path
fn to_sfile(file: &str, root_dir: &Path, sccs_dir: &str) -> PathBuf {
    let path = Path::new(file);

    // If already an s-file, use as-is (but prepend root_dir)
    if let Some(name) = path.file_name() {
        if name.to_string_lossy().starts_with("s.") {
            return root_dir.join(path);
        }
    }

    // Get directory and filename components
    let dir = path.parent().unwrap_or(Path::new(""));
    let name = path
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_default();

    // Build: root_dir / dir / sccs_dir / s.name, or root_dir / dir / s.name
    // when there is no SCCS directory to hold it.
    //
    // The fallback is what plib::sccsfile::paths::sfile_from_gfile does, and
    // what the utilities themselves therefore accept. Always inserting the
    // SCCS component left the front end unable to address a flat directory
    // that `get s.f` handles perfectly well.
    let mut base = root_dir.to_path_buf();
    if !dir.as_os_str().is_empty() {
        base = base.join(dir);
    }
    let sname = format!("s.{}", name);
    let in_sccs_dir = base.join(sccs_dir);
    if in_sccs_dir.is_dir() {
        in_sccs_dir.join(sname)
    } else {
        base.join(sname)
    }
}

/// Whether `-<letter>` for `cmd` takes an option-argument that may be given as
/// a separate token.
///
/// The front end used to split argv on a leading '-', which made the value of
/// any separated option a file operand: `sccs get -r 1.1 f` reached get as
/// `-r SCCS/s.f SCCS/s.1.1`. Inventing an operand is a worse failure than
/// diagnosing a missing argument.
///
/// The table mirrors how each sibling actually parses, which is the property
/// that matters -- the front end must not classify a token differently from
/// the utility it is about to hand it to. `prs -r` is absent deliberately: its
/// argument is optional and attached-only, and prs rewrites a bare `-r` so
/// clap cannot swallow the following operand. `admin`'s -i, -t and -y are
/// absent for the same reason; they are pre-parsed as attached-only forms.
fn option_takes_value(cmd: &str, letter: char) -> bool {
    let letters = match cmd {
        "admin" => "rmfdae",
        "get" => "rcix",
        "delta" => "rymg",
        "prs" => "cd",
        "rmdel" | "unget" => "r",
        "val" => "mry",
        _ => "",
    };
    letters.contains(letter)
}

/// Split argv into options (with their arguments) and file operands.
fn split_options<'a>(cmd: &str, args: &[&'a str]) -> (Vec<&'a str>, Vec<&'a str>) {
    let mut opts = Vec::new();
    let mut files = Vec::new();
    let mut it = args.iter().copied();

    while let Some(arg) = it.next() {
        // "--" ends the options; everything after is an operand.
        if arg == "--" {
            files.extend(it);
            break;
        }
        if arg == "-" || !arg.starts_with('-') {
            files.push(arg);
            continue;
        }

        opts.push(arg);

        // Short options cluster, so scan for the first letter that takes a
        // value. If it ends the token, its argument is the next token.
        let mut chars = arg[1..].chars();
        let mut consumes = false;
        for c in chars.by_ref() {
            if option_takes_value(cmd, c) {
                // Anything left in this token is the attached argument.
                consumes = chars.as_str().is_empty();
                break;
            }
        }
        if consumes {
            if let Some(value) = it.next() {
                opts.push(value);
            }
        }
    }

    (opts, files)
}

/// The source file a `sccs create` operand names, relative to the -d root.
///
/// This is the file the user is handing to admin, not a g-file that get
/// produced, so it is the operand as given.
fn to_source_file(file: &str, root_dir: &Path) -> PathBuf {
    root_dir.join(file)
}

/// The working file a retrieval of `sfile` produces.
///
/// `get` writes the g-file as a bare name in the current directory, and
/// `delta` and `unget` look for it there. The front end computed
/// `root_dir.join(file)` instead, so under -d it addressed a different file
/// than the utilities it drives -- the exact placement bug plib's
/// gfile_from_sfile was fixed for. Asking plib is what keeps them agreed.
fn to_gfile(sfile: &Path) -> PathBuf {
    paths::gfile_from_sfile(sfile).unwrap_or_else(|| PathBuf::from("."))
}

/// One editing record, mirroring a p-file entry:
/// (g-file, old-SID, new-SID, user, date, time).
struct EditInfo {
    gfile: String,
    old_sid: String,
    new_sid: String,
    user: String,
    date: String,
    time: String,
}

fn get_editing_info(
    root_dir: &Path,
    sccs_dir: &str,
    branch_only: bool,
    user_filter: Option<&str>,
) -> Vec<EditInfo> {
    let mut results = Vec::new();

    let sccs_path = root_dir.join(sccs_dir);
    if let Ok(entries) = fs::read_dir(&sccs_path) {
        for entry in entries.flatten() {
            let path = entry.path();
            if let Some(name) = path.file_name() {
                let name_str = name.to_string_lossy();
                if name_str.starts_with("p.") {
                    // Parse with the shared p-file reader rather than by hand.
                    // The hand-rolled version required five whitespace fields
                    // where PfileEntry::parse requires four and defaults the
                    // time, so a line every other utility accepts was silently
                    // dropped here -- and it detected a branch by counting '.'
                    // characters instead of asking the SID.
                    let entries = fs::read_to_string(&path)
                        .ok()
                        .and_then(|c| plib::sccsfile::parse_pfile(&c).ok())
                        .unwrap_or_default();

                    for entry in entries {
                        if branch_only && entry.new_sid.is_trunk() {
                            continue;
                        }
                        if let Some(u) = user_filter {
                            if entry.user != u {
                                continue;
                            }
                        }

                        results.push(EditInfo {
                            gfile: name_str.strip_prefix("p.").unwrap().to_string(),
                            old_sid: entry.old_sid.to_string(),
                            new_sid: entry.new_sid.to_string(),
                            user: entry.user,
                            date: entry.datetime.date_string(),
                            time: entry.datetime.time_string(),
                        });
                    }
                }
            }
        }
    }

    results
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        usage();
    }

    let mut root_dir = PathBuf::from(".");
    let mut sccs_dir = "SCCS".to_string();
    let mut use_real_uid = false;
    let mut arg_idx = 1;

    // Parse sccs-specific options
    while arg_idx < args.len() {
        let arg = &args[arg_idx];
        if arg == "-r" {
            use_real_uid = true;
            arg_idx += 1;
        } else if arg == "-d" {
            arg_idx += 1;
            if arg_idx >= args.len() {
                usage();
            }
            root_dir = PathBuf::from(&args[arg_idx]);
            arg_idx += 1;
        } else if let Some(stripped) = arg.strip_prefix("-d") {
            root_dir = PathBuf::from(stripped);
            arg_idx += 1;
        } else if arg == "-p" {
            arg_idx += 1;
            if arg_idx >= args.len() {
                usage();
            }
            sccs_dir = args[arg_idx].clone();
            arg_idx += 1;
        } else if let Some(stripped) = arg.strip_prefix("-p") {
            sccs_dir = stripped.to_string();
            arg_idx += 1;
        } else if arg.starts_with("-") {
            // Unknown option - might be for the subcommand
            break;
        } else {
            break;
        }
    }

    // Check PROJECTDIR if -d not specified and root_dir is still default
    if root_dir == Path::new(".") {
        if let Ok(projectdir) = env::var("PROJECTDIR") {
            if projectdir.starts_with('/') {
                root_dir = PathBuf::from(projectdir);
            } else if let Some(home) = user_home_dir(&projectdir) {
                // Treat as a user name: examine that user's home directory for a
                // `src` or `source` subdirectory.
                let src = home.join("src");
                let source = home.join("source");
                if src.is_dir() {
                    root_dir = src;
                } else if source.is_dir() {
                    root_dir = source;
                }
            }
        }
    }

    if arg_idx >= args.len() {
        usage();
    }

    let command = &args[arg_idx];
    arg_idx += 1;

    // Collect remaining args (options and operands for the command)
    let remaining_args: Vec<&str> = args[arg_idx..].iter().map(|s| s.as_str()).collect();

    // Handle pseudo-utilities
    match command.as_str() {
        "edit" => {
            // Equivalent to get -e
            run_sccs_command(
                "get",
                &["-e"],
                &remaining_args,
                &root_dir,
                &sccs_dir,
                use_real_uid,
            )
        }

        "unedit" => {
            // Equivalent to unget
            run_sccs_command(
                "unget",
                &[],
                &remaining_args,
                &root_dir,
                &sccs_dir,
                use_real_uid,
            )
        }

        "delget" => {
            // delta then get.  Per spec: -m -p -r -s -y -> delta;
            // -b -c -e -i -k -l -s -x -> get.
            let (delta_opts, get_opts) = split_delget_opts(&remaining_args, "mprsy", "bceiklsx");

            let code = run_sccs_command(
                "delta",
                &[],
                &delta_opts,
                &root_dir,
                &sccs_dir,
                use_real_uid,
            );
            if code != ExitCode::SUCCESS {
                return code;
            }
            run_sccs_command("get", &[], &get_opts, &root_dir, &sccs_dir, use_real_uid)
        }

        "deledit" => {
            // delta then get -e.  Same partition as delget, but the get phase
            // is forced into -e mode, so -e is not routed from the argv here.
            let (delta_opts, get_opts) = split_delget_opts(&remaining_args, "mprsy", "bciklsx");

            let code = run_sccs_command(
                "delta",
                &[],
                &delta_opts,
                &root_dir,
                &sccs_dir,
                use_real_uid,
            );
            if code != ExitCode::SUCCESS {
                return code;
            }
            run_sccs_command(
                "get",
                &["-e"],
                &get_opts,
                &root_dir,
                &sccs_dir,
                use_real_uid,
            )
        }

        "create" => {
            // admin -i then rename original
            let files: Vec<&str> = remaining_args
                .iter()
                .filter(|a| !a.starts_with("-"))
                .copied()
                .collect();
            let opts: Vec<&str> = remaining_args
                .iter()
                .filter(|a| a.starts_with("-"))
                .copied()
                .collect();

            for file in &files {
                let sfile = to_sfile(file, &root_dir, &sccs_dir);
                let gfile = to_source_file(file, &root_dir);

                // Ensure SCCS directory exists
                if let Some(parent) = sfile.parent() {
                    fs::create_dir_all(parent).ok();
                }

                // Run admin -i<gfile> <sfile>
                let mut cmd_args = opts.clone();
                let init_arg = format!("-i{}", gfile.display());
                cmd_args.push(&init_arg);

                if use_real_uid {
                    drop_privileges();
                }
                let status = Command::new(sibling("admin"))
                    .args(&cmd_args)
                    .arg(&sfile)
                    .status();

                match status {
                    Ok(s) if s.success() => {
                        // Rename original file to ,file
                        let backup = gfile
                            .parent()
                            .unwrap_or(Path::new("."))
                            .join(format!(",{}", gfile.file_name().unwrap().to_string_lossy()));
                        fs::rename(&gfile, backup).ok();
                    }
                    Ok(s) => {
                        return ExitCode::from(s.code().unwrap_or(1) as u8);
                    }
                    Err(e) => {
                        eprintln!("sccs: admin: {}", e);
                        return ExitCode::FAILURE;
                    }
                }
            }
            ExitCode::SUCCESS
        }

        "fix" => {
            // Remove delta but leave copy with changes
            // Requires -r SID
            let sid_opt = remaining_args
                .iter()
                .find(|a| a.starts_with("-r"))
                .map(|s| &s[2..]);

            if sid_opt.is_none() {
                eprintln!("sccs: {}", gettext("fix requires -r SID"));
                return ExitCode::FAILURE;
            }

            let files: Vec<&str> = remaining_args
                .iter()
                .filter(|a| !a.starts_with("-"))
                .copied()
                .collect();

            for file in &files {
                let sfile = to_sfile(file, &root_dir, &sccs_dir);

                if use_real_uid {
                    drop_privileges();
                }

                // Get the version being fixed
                let status = Command::new(sibling("get"))
                    .arg("-k")
                    .arg(format!("-r{}", sid_opt.unwrap()))
                    .arg(&sfile)
                    .status();

                if let Ok(s) = status {
                    if !s.success() {
                        return ExitCode::from(s.code().unwrap_or(1) as u8);
                    }
                }

                // Remove the delta
                let status = Command::new(sibling("rmdel"))
                    .arg(format!("-r{}", sid_opt.unwrap()))
                    .arg(&sfile)
                    .status();

                if let Ok(s) = status {
                    if !s.success() {
                        return ExitCode::from(s.code().unwrap_or(1) as u8);
                    }
                }
            }
            ExitCode::SUCCESS
        }

        "info" | "check" | "tell" => {
            // List files being edited
            let mut branch_only = false;
            let mut user_filter: Option<String> = None;

            for arg in &remaining_args {
                if *arg == "-b" {
                    branch_only = true;
                } else if let Some(stripped) = arg.strip_prefix("-u") {
                    user_filter = Some(stripped.to_string());
                } else if *arg == "-U" {
                    user_filter = Some(plib::sccsfile::real_login_name());
                }
            }

            let info = get_editing_info(&root_dir, &sccs_dir, branch_only, user_filter.as_deref());

            if command == "check" {
                if info.is_empty() {
                    ExitCode::SUCCESS
                } else {
                    ExitCode::FAILURE
                }
            } else if command == "tell" {
                for e in &info {
                    println!("{}", e.gfile);
                }
                ExitCode::SUCCESS
            } else {
                // info: report the full p-file detail
                // (old-SID new-SID user date time).
                if info.is_empty() {
                    println!("{}", gettext("Nothing being edited."));
                } else {
                    for e in &info {
                        println!(
                            "{}: {} {} {} {} {} {}",
                            e.gfile,
                            gettext("being edited:"),
                            e.old_sid,
                            e.new_sid,
                            e.user,
                            e.date,
                            e.time
                        );
                    }
                }
                ExitCode::SUCCESS
            }
        }

        "clean" => {
            // Remove files that can be recreated from SCCS
            let branch_only = remaining_args.contains(&"-b");
            let info = get_editing_info(&root_dir, &sccs_dir, branch_only, None);
            let editing: Vec<&str> = info.iter().map(|e| e.gfile.as_str()).collect();

            // Find g-files in current directory
            if let Ok(entries) = fs::read_dir(&root_dir) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.is_file() {
                        if let Some(name) = path.file_name() {
                            let name_str = name.to_string_lossy();
                            // Check if corresponding s-file exists
                            let sfile = root_dir.join(&sccs_dir).join(format!("s.{}", name_str));
                            if sfile.exists() && !editing.contains(&name_str.as_ref()) {
                                fs::remove_file(&path).ok();
                            }
                        }
                    }
                }
            }
            ExitCode::SUCCESS
        }

        "diffs" => {
            // Diff current vs SCCS version
            let files: Vec<&str> = remaining_args
                .iter()
                .filter(|a| !a.starts_with("-"))
                .copied()
                .collect();

            let get_opts: Vec<&str> = remaining_args
                .iter()
                .filter(|a| {
                    a.starts_with("-r")
                        || a.starts_with("-c")
                        || a.starts_with("-i")
                        || a.starts_with("-x")
                        || a.starts_with("-t")
                })
                .copied()
                .collect();

            let mut diff_opts: Vec<&str> = remaining_args
                .iter()
                .filter(|a| {
                    a.starts_with("-l")
                        || a.starts_with("-s")
                        || a.starts_with("-e")
                        || a.starts_with("-f")
                        || a.starts_with("-h")
                        || a.starts_with("-b")
                })
                .copied()
                .collect();

            // Handle -C -> -c for diff
            if remaining_args.contains(&"-C") {
                diff_opts.push("-c");
            }

            if use_real_uid {
                drop_privileges();
            }

            for file in files.iter() {
                let sfile = to_sfile(file, &root_dir, &sccs_dir);
                let gfile = to_gfile(&sfile);

                // The retrieved text goes to a temporary file created with
                // mkstemp. A name built from the pid was guessable, and the
                // plain create-and-truncate that followed would happily write
                // through a symlink an attacker had left in its place -- in a
                // binary that carries drop_privileges() precisely because it
                // may be installed setuid.
                let mut tmp = match plib::tmp::Builder::new().prefix("sccs_diff.").tempfile() {
                    Ok(t) => t,
                    Err(e) => {
                        eprintln!("sccs: {}", e);
                        return ExitCode::FAILURE;
                    }
                };

                let mut get_cmd = Command::new(sibling("get"));
                get_cmd.args(&get_opts).arg("-p").arg("-s").arg(&sfile);

                if let Ok(o) = get_cmd.output() {
                    if tmp.as_file_mut().write_all(&o.stdout).is_err() {
                        continue;
                    }

                    // Run diff (a system tool, not an SCCS sibling).
                    Command::new("diff")
                        .args(&diff_opts)
                        .arg(tmp.path())
                        .arg(&gfile)
                        .status()
                        .ok();
                }
            }
            ExitCode::SUCCESS
        }

        "print" => {
            // Equivalent to prs (verbose info)
            run_sccs_command(
                "prs",
                &[],
                &remaining_args,
                &root_dir,
                &sccs_dir,
                use_real_uid,
            )
        }

        // Standard SCCS utilities
        "admin" | "delta" | "get" | "prs" | "rmdel" | "sact" | "unget" | "val" | "what" => {
            run_sccs_command(
                command,
                &[],
                &remaining_args,
                &root_dir,
                &sccs_dir,
                use_real_uid,
            )
        }

        _ => {
            eprintln!("sccs: {} '{}'", gettext("unknown command"), command);
            ExitCode::FAILURE
        }
    }
}

fn run_sccs_command(
    cmd: &str,
    extra_opts: &[&str],
    args: &[&str],
    root_dir: &Path,
    sccs_dir: &str,
    use_real_uid: bool,
) -> ExitCode {
    // Separate options (with their arguments) from file operands.
    let (opts, files) = split_options(cmd, args);

    // Convert file operands to s-file paths
    let sfiles: Vec<PathBuf> = files
        .iter()
        .map(|f| to_sfile(f, root_dir, sccs_dir))
        .collect();

    // When -r was requested, drop to the real uid/gid before spawning.
    if use_real_uid {
        drop_privileges();
    }

    let mut command = Command::new(sibling(cmd));
    command.args(extra_opts);
    command.args(&opts);
    for sfile in &sfiles {
        command.arg(sfile);
    }

    match command.status() {
        Ok(status) => {
            if status.success() {
                ExitCode::SUCCESS
            } else {
                ExitCode::from(status.code().unwrap_or(1) as u8)
            }
        }
        Err(e) => {
            eprintln!("sccs: {}: {}", cmd, e);
            ExitCode::FAILURE
        }
    }
}
