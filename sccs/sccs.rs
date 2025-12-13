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
use std::path::{Path, PathBuf};
use std::process::{self, Command, ExitCode};

use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};

fn usage() -> ! {
    eprintln!("usage: sccs [-r] [-d path] [-p path] command [options...] [operands...]");
    process::exit(2);
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

    // Build: root_dir / dir / sccs_dir / s.name
    let mut result = root_dir.to_path_buf();
    if !dir.as_os_str().is_empty() {
        result = result.join(dir);
    }
    result = result.join(sccs_dir);
    result = result.join(format!("s.{}", name));

    result
}

/// Convert a file operand to g-file path (working file)
fn to_gfile(file: &str, root_dir: &Path) -> PathBuf {
    root_dir.join(file)
}

/// Get list of files being edited (from p-files in SCCS directory)
fn get_editing_info(
    root_dir: &Path,
    sccs_dir: &str,
    branch_only: bool,
    user_filter: Option<&str>,
) -> Vec<(String, String, String)> {
    let mut results = Vec::new();

    let sccs_path = root_dir.join(sccs_dir);
    if let Ok(entries) = fs::read_dir(&sccs_path) {
        for entry in entries.flatten() {
            let path = entry.path();
            if let Some(name) = path.file_name() {
                let name_str = name.to_string_lossy();
                if name_str.starts_with("p.") {
                    // Read p-file
                    if let Ok(contents) = fs::read_to_string(&path) {
                        for line in contents.lines() {
                            let parts: Vec<&str> = line.split_whitespace().collect();
                            if parts.len() >= 5 {
                                let sid = parts[1];
                                let user = parts[2];

                                // Filter by branch
                                if branch_only {
                                    // Branch SIDs have 4 components (R.L.B.S)
                                    if sid.matches('.').count() < 3 {
                                        continue;
                                    }
                                }

                                // Filter by user
                                if let Some(u) = user_filter {
                                    if user != u {
                                        continue;
                                    }
                                }

                                let gfile = name_str.strip_prefix("p.").unwrap().to_string();
                                results.push((gfile, sid.to_string(), user.to_string()));
                            }
                        }
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
    if root_dir == PathBuf::from(".") {
        if let Ok(projectdir) = env::var("PROJECTDIR") {
            if projectdir.starts_with('/') {
                root_dir = PathBuf::from(projectdir);
            } else {
                // Treat as username - look for their src or source directory
                if let Ok(home) = env::var("HOME") {
                    let src = PathBuf::from(&home).join(&projectdir).join("src");
                    let source = PathBuf::from(&home).join(&projectdir).join("source");
                    if src.is_dir() {
                        root_dir = src;
                    } else if source.is_dir() {
                        root_dir = source;
                    } else {
                        root_dir = PathBuf::from(projectdir);
                    }
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
            // delta then get
            let delta_opts: Vec<&str> = remaining_args
                .iter()
                .filter(|a| {
                    a.starts_with("-m")
                        || a.starts_with("-p")
                        || a.starts_with("-r")
                        || a.starts_with("-s")
                        || a.starts_with("-y")
                        || !a.starts_with("-")
                })
                .copied()
                .collect();
            let get_opts: Vec<&str> = remaining_args
                .iter()
                .filter(|a| {
                    a.starts_with("-b")
                        || a.starts_with("-c")
                        || a.starts_with("-e")
                        || a.starts_with("-i")
                        || a.starts_with("-k")
                        || a.starts_with("-l")
                        || a.starts_with("-s")
                        || a.starts_with("-x")
                        || !a.starts_with("-")
                })
                .copied()
                .collect();

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
            // delta then get -e
            let delta_opts: Vec<&str> = remaining_args
                .iter()
                .filter(|a| {
                    a.starts_with("-m")
                        || a.starts_with("-p")
                        || a.starts_with("-r")
                        || a.starts_with("-s")
                        || a.starts_with("-y")
                        || !a.starts_with("-")
                })
                .copied()
                .collect();
            let get_opts: Vec<&str> = remaining_args
                .iter()
                .filter(|a| {
                    a.starts_with("-b")
                        || a.starts_with("-c")
                        || a.starts_with("-i")
                        || a.starts_with("-k")
                        || a.starts_with("-l")
                        || a.starts_with("-s")
                        || a.starts_with("-x")
                        || !a.starts_with("-")
                })
                .copied()
                .collect();

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
                let gfile = to_gfile(file, &root_dir);

                // Ensure SCCS directory exists
                if let Some(parent) = sfile.parent() {
                    fs::create_dir_all(parent).ok();
                }

                // Run admin -i<gfile> <sfile>
                let mut cmd_args = opts.clone();
                let init_arg = format!("-i{}", gfile.display());
                cmd_args.push(&init_arg);

                let status = Command::new("admin").args(&cmd_args).arg(&sfile).status();

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
                eprintln!("sccs: fix requires -r SID");
                return ExitCode::FAILURE;
            }

            let files: Vec<&str> = remaining_args
                .iter()
                .filter(|a| !a.starts_with("-"))
                .copied()
                .collect();

            for file in &files {
                let sfile = to_sfile(file, &root_dir, &sccs_dir);

                // Get the version being fixed
                let status = Command::new("get")
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
                let status = Command::new("rmdel")
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
                    user_filter = Some(
                        env::var("LOGNAME")
                            .or_else(|_| env::var("USER"))
                            .unwrap_or_default(),
                    );
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
                for (file, _sid, _user) in &info {
                    println!("{}", file);
                }
                ExitCode::SUCCESS
            } else {
                // info
                if info.is_empty() {
                    println!("Nothing being edited.");
                } else {
                    for (file, sid, user) in &info {
                        println!("{}: being edited: {} {}", file, sid, user);
                    }
                }
                ExitCode::SUCCESS
            }
        }

        "clean" => {
            // Remove files that can be recreated from SCCS
            let branch_only = remaining_args.contains(&"-b");
            let info = get_editing_info(&root_dir, &sccs_dir, branch_only, None);
            let editing: Vec<&str> = info.iter().map(|(f, _, _)| f.as_str()).collect();

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

            for file in &files {
                let sfile = to_sfile(file, &root_dir, &sccs_dir);
                let gfile = to_gfile(file, &root_dir);

                // Get SCCS version to temp file
                let tmp = format!("/tmp/sccs_diff.{}", std::process::id());
                let mut get_cmd = Command::new("get");
                get_cmd.args(&get_opts).arg("-p").arg("-s").arg(&sfile);

                let output = get_cmd.output();
                if let Ok(o) = output {
                    fs::write(&tmp, &o.stdout).ok();

                    // Run diff
                    Command::new("diff")
                        .args(&diff_opts)
                        .arg(&tmp)
                        .arg(&gfile)
                        .status()
                        .ok();

                    fs::remove_file(&tmp).ok();
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
            eprintln!("sccs: unknown command '{}'", command);
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
    _use_real_uid: bool,
) -> ExitCode {
    // Separate options from file operands
    let (opts, files): (Vec<&str>, Vec<&str>) = args.iter().partition(|a| a.starts_with("-"));

    // Convert file operands to s-file paths
    let sfiles: Vec<PathBuf> = files
        .iter()
        .map(|f| to_sfile(f, root_dir, sccs_dir))
        .collect();

    let mut command = Command::new(cmd);
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
