//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use regex::Regex;
use std::collections::HashSet;
use std::os::unix::fs::{FileTypeExt, MetadataExt, PermissionsExt};
use std::path::PathBuf;
use std::{env, fs};
use walkdir::{DirEntry, WalkDir};

#[derive(Debug, Clone)]
enum Expr {
    And(Box<Expr>),
    Or(Box<Expr>),
    Not(Box<Expr>),
    Name(String),
    MTime(i64),
    Path(String),
    Type(FileType),
    NoUser,
    NoGroup,
    XDev,
    Prune,
    Perm(u32),
    Links(u64),
    User(String),
    Group(String),
    Size(u64, bool),
    Print,
    Newer(PathBuf),
}

#[derive(Debug, Clone)]
enum FileType {
    BlockDevice,
    CharDevice,
    Dir,
    Symlink,
    Fifo,
    File,
    Socket,
    Unknown,
}

/// Parses a list of tokens representing search criteria, the result is stored in enum Expr
///
/// # Arguments
///
/// * `tokens` - A vector of string slices representing the tokens to parse.
///
/// # Returns
///
/// * A vector of `Expr` expressions parsed from the tokens.
fn parse_expression(tokens: &mut Vec<&str>) -> Vec<Expr> {
    let mut stack: Vec<Expr> = Vec::new();

    while let Some(&token) = tokens.last() {
        match token {
            "-name" => {
                tokens.pop();
                if let Some(name) = tokens.pop() {
                    stack.push(Expr::Name(name.to_string()));
                }
            }
            "-path" => {
                tokens.pop();
                if let Some(path) = tokens.pop() {
                    stack.push(Expr::Path(path.to_string()));
                }
            }
            "-mtime" => {
                tokens.pop();
                if let Some(mtime) = tokens.pop() {
                    if let Ok(mtime) = mtime.parse::<i64>() {
                        stack.push(Expr::MTime(mtime));
                    }
                }
            }
            "-type" => {
                tokens.pop();
                if let Some(t) = tokens.pop() {
                    if t.len() == 1 {
                        let filetype = match t {
                            "b" => FileType::BlockDevice,
                            "c" => FileType::CharDevice,
                            "d" => FileType::Dir,
                            "l" => FileType::Symlink,
                            "p" => FileType::Fifo,
                            "f" => FileType::File,
                            "s" => FileType::Socket,
                            _ => FileType::Unknown,
                        };
                        stack.push(Expr::Type(filetype));
                    }
                }
            }
            "-nouser" => {
                tokens.pop();
                stack.push(Expr::NoUser);
            }
            "-nogroup" => {
                tokens.pop();
                stack.push(Expr::NoGroup);
            }
            "-xdev" => {
                tokens.pop();
                stack.push(Expr::XDev);
            }
            "-prune" => {
                tokens.pop();
                stack.push(Expr::Prune);
            }
            "-perm" => {
                tokens.pop();
                if let Some(perm) = tokens.pop() {
                    if let Ok(perm) = u32::from_str_radix(perm, 8) {
                        stack.push(Expr::Perm(perm));
                    }
                }
            }
            "-links" => {
                tokens.pop();
                if let Some(links) = tokens.pop() {
                    if let Ok(links) = links.parse::<u64>() {
                        stack.push(Expr::Links(links));
                    }
                }
            }
            "-user" => {
                tokens.pop();
                if let Some(user) = tokens.pop() {
                    stack.push(Expr::User(user.to_string()));
                }
            }
            "-group" => {
                tokens.pop();
                if let Some(group) = tokens.pop() {
                    stack.push(Expr::Group(group.to_string()));
                }
            }
            "-size" => {
                tokens.pop();
                if let Some(size) = tokens.pop() {
                    let (size, in_bytes) = if size.ends_with('c') {
                        (size[..size.len() - 1].parse::<u64>().unwrap_or(0), true)
                    } else {
                        (size.parse::<u64>().unwrap_or(0), false)
                    };
                    stack.push(Expr::Size(size, in_bytes));
                }
            }
            "-newer" => {
                tokens.pop();
                if let Some(file) = tokens.pop() {
                    stack.push(Expr::Newer(PathBuf::from(file)));
                }
            }
            "-print" => {
                tokens.pop();
                stack.push(Expr::Print);
            }
            "-a" => {
                tokens.pop();
                let expr = parse_expression(tokens);
                stack.push(Expr::And(Box::new(expr[0].clone())));
            }
            "-o" => {
                tokens.pop();
                let expr = parse_expression(tokens);
                stack.push(Expr::Or(Box::new(expr[0].clone())));
            }
            "!" => {
                tokens.pop();
                let expr = parse_expression(tokens);
                stack.push(Expr::Not(Box::new(expr[0].clone())));
            }
            _ => {
                tokens.pop();
                stack.push(Expr::Path(
                    PathBuf::from(token).to_string_lossy().to_string(),
                ));
            }
        }
    }

    stack
}

/// Converts a shell pattern to a regular expression.
///
/// # Arguments
///
/// * `pattern` - A string slice representing the pattern to convert.
///
/// # Returns
///
/// * A `Regex` object representing the converted pattern.
fn pattern_to_regex(pattern: &str) -> Regex {
    let mut regex_pattern = pattern.replace('?', ".");
    regex_pattern = regex_pattern.replace('*', ".*");

    let bracket_regex = Regex::new(r"\[(?:[^\]]+)\]").unwrap();
    regex_pattern = bracket_regex
        .replace_all(&regex_pattern, |caps: &regex::Captures| {
            let bracket_content = &caps[0][1..caps[0].len() - 1];
            format!("[{}]", bracket_content)
        })
        .to_string();

    Regex::new(&format!("^{}$", regex_pattern)).unwrap()
}

/// Executes a command based on the list of expressions and returns the matching file paths.
///
/// # Arguments
///
/// * `expr` - A slice of `Expr` expressions to execute.
/// * `files` - A vector of `DirEntry` objects representing the files to be evaluated.
/// * `root_dev` - A u64 value representing the root device number for `-xdev` expression.
///
/// # Returns
///
/// * A `Result` containing a vector of `PathBuf` objects representing the paths of the files that match the expressions,
///   or an error message as a `String`.
fn evaluate_expression(
    expr: &[Expr],
    files: Vec<DirEntry>,
    root_dev: u64,
) -> Result<Vec<PathBuf>, String> {
    let f_path = &expr[0];
    let mut not_res: Vec<PathBuf> = Vec::new();
    let mut or_res: Vec<PathBuf> = Vec::new();
    let mut and_res: Vec<PathBuf> = Vec::new();
    let mut c_files = files
        .clone()
        .into_iter()
        .map(|f| f.path().to_path_buf())
        .collect::<HashSet<PathBuf>>();
    let mut result = Vec::new();
    let mut first = true;
    for expression in expr {
        match expression {
            Expr::Not(inner) => {
                let i: Vec<Expr> = vec![f_path.clone(), *inner.clone()];
                not_res = evaluate_expression(&i.as_slice(), files.clone(), root_dev)?;
            }
            Expr::Or(inner) => {
                let i: Vec<Expr> = vec![f_path.clone(), *inner.clone()];
                or_res = evaluate_expression(&i.as_slice(), files.clone(), root_dev)?;
            }
            Expr::And(inner) => {
                let i: Vec<Expr> = vec![f_path.clone(), *inner.clone()];
                and_res = evaluate_expression(&i.as_slice(), files.clone(), root_dev)?;
            }
            _ => {}
        }
        for file in &files {
            match expression {
                Expr::And(_) => {
                    continue;
                }
                Expr::Or(_) => {
                    continue;
                }
                Expr::Not(_) => {
                    continue;
                }
                Expr::Name(name) => {
                    let regex = pattern_to_regex(name);
                    if !regex.is_match(&file.file_name().to_string_lossy()) {
                        c_files.remove(file.path());
                    }
                }
                Expr::Path(path) => {
                    let regex = pattern_to_regex(path);

                    if !regex.is_match(&file.path().to_string_lossy()) && !first {
                        c_files.remove(file.path());
                    }
                }
                Expr::MTime(days) => {
                    if let Ok(metadata) = file.metadata() {
                        let modified = metadata.modified().unwrap();
                        let duration = std::time::SystemTime::now()
                            .duration_since(modified)
                            .unwrap();
                        if ((duration.as_secs() / 86400) as i64) < (*days) {
                            c_files.remove(file.path());
                        }
                    }
                }
                Expr::Type(t) => {
                    let file_type = file.file_type();
                    let r = match *t {
                        FileType::BlockDevice => file_type.is_block_device(),
                        FileType::CharDevice => file_type.is_char_device(),
                        FileType::Dir => file_type.is_dir(),
                        FileType::Symlink => file_type.is_symlink(),
                        FileType::Fifo => file_type.is_fifo(),
                        FileType::File => file_type.is_file(),
                        FileType::Socket => file_type.is_socket(),
                        FileType::Unknown => return Err(format!("Unknown argument to -type")),
                    };
                    if !r {
                        c_files.remove(file.path());
                    }
                }
                Expr::NoUser => {
                    if let Ok(metadata) = file.metadata() {
                        let uid = metadata.uid();
                        if !users::get_user_by_uid(uid).is_none() {
                            c_files.remove(file.path());
                        }
                    }
                }
                Expr::NoGroup => {
                    if let Ok(metadata) = file.metadata() {
                        let gid = metadata.gid();
                        if !users::get_group_by_gid(gid).is_none() {
                            c_files.remove(file.path());
                        }
                    }
                }
                Expr::XDev => {
                    if let Ok(metadata) = file.metadata() {
                        if metadata.dev() != root_dev {
                            c_files.remove(file.path());
                        }
                    }
                }
                Expr::Prune => {}
                Expr::Perm(perm) => {
                    if let Ok(metadata) = file.metadata() {
                        if metadata.permissions().mode() & 0o777 != *perm {
                            c_files.remove(file.path());
                        }
                    }
                }
                Expr::Links(links) => {
                    if let Ok(metadata) = file.metadata() {
                        if metadata.nlink() != *links {
                            c_files.remove(file.path());
                        }
                    }
                }
                Expr::User(user) => {
                    if let Ok(metadata) = file.metadata() {
                        let uid = metadata.uid();
                        if let Ok(parsed_uid) = user.parse::<u32>() {
                            if uid != parsed_uid {
                                c_files.remove(file.path());
                            }
                        }
                    }
                }
                Expr::Group(group) => {
                    if let Ok(metadata) = file.metadata() {
                        let gid = metadata.gid();
                        if let Ok(parsed_gid) = group.parse::<u32>() {
                            if gid != parsed_gid {
                                c_files.remove(file.path());
                            }
                        } else {
                            c_files.remove(file.path());
                        }
                    }
                }
                Expr::Size(size, in_bytes) => {
                    if let Ok(metadata) = file.metadata() {
                        let file_size = if *in_bytes {
                            metadata.len()
                        } else {
                            (metadata.len() + 511) / 512
                        };
                        if file_size < *size {
                            c_files.remove(file.path());
                        }
                    }
                }
                Expr::Newer(f) => {
                    if let Ok(metadata) = fs::metadata(f) {
                        if let Ok(file_metadata) = file.metadata() {
                            if !(file_metadata.modified().unwrap() > metadata.modified().unwrap()) {
                                c_files.remove(file.path());
                            }
                        }
                    }
                }
                Expr::Print if c_files.contains(file.path()) => {
                    result.push(file.path().to_path_buf());
                }
                Expr::Print if !c_files.contains(file.path()) => {
                    continue;
                }
                _ => return Err("Error: Invalid expression".to_string()),
            }
        }
        first = false;
    }

    if result.is_empty() {
        result.extend(c_files.clone());
    }

    let set: std::collections::HashSet<_> = not_res.iter().cloned().collect();
    result.retain(|x| !set.contains(x));

    result.extend(or_res);

    let and_set: std::collections::HashSet<_> = and_res.iter().cloned().collect();
    if !and_set.is_empty() {
        result.retain(|x| and_set.contains(x));
    }

    result.sort();
    Ok(result)
}

/// Retrieves the root path from a list of expressions.
///
/// # Arguments
///
/// * `expr` - A slice of `Expr` expressions.
///
/// # Returns
///
/// * A `String` representing the root path extracted from the expressions.
fn get_root(expr: &[Expr]) -> String {
    let mut first = true;

    let path = expr
        .iter()
        .find_map(|i| match i {
            Expr::Path(p) if first => {
                first = false;
                Some(p.to_string())
            }
            _ => None,
        })
        .unwrap_or_else(String::new);

    path
}

/// Executes the find command with the provided arguments.
///
/// # Arguments
///
/// * `args` - A vector of `String` representing the command-line arguments.
///
/// # Returns
///
/// * A `Result` indicating success or containing an error message as a `String`.
fn find(args: Vec<String>) -> Result<(), String> {
    let mut tokens: Vec<&str> = args.iter().skip(1).rev().map(|s| s.as_str()).collect();
    let binding = parse_expression(&mut tokens);
    let expr = binding.as_slice();
    let path = get_root(expr);

    let root_dev = if let Ok(metadata) = fs::metadata(path.clone()) {
        metadata.dev()
    } else {
        return Err("Error: Could not retrieve root device metadata".to_string());
    };

    let files = WalkDir::new(path)
        .into_iter()
        .map(|f| f.unwrap())
        .collect::<Vec<DirEntry>>();
    let result = evaluate_expression(expr, files, root_dev);

    for res in result? {
        println!("{}", res.display())
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let args: Vec<String> = env::args().collect();

    let mut exit_code = 0;

    if let Err(err) = find(args) {
        exit_code = 1;
        eprint!("{}", err);
    }

    std::process::exit(exit_code)
}
