//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::BTreeMap;
use std::env;
use std::ffi::{OsStr, OsString};
use std::io::{self, BufWriter, Write};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};

use clap::Parser;
use gettextrs::gettext;
use plib::diag;
use posixutils_process::exec::exec_error_exit;

#[derive(Parser)]
#[command(
    version,
    disable_help_flag = true,
    disable_version_flag = true,
    about = gettext("env - set the environment for command invocation")
)]
struct Args {
    #[arg(
        short = 'i',
        help = gettext(
            "Invoke utility with exactly the environment specified by the arguments; the inherited environment shall be ignored completely"
        )
    )]
    ignore_env: bool,

    // See the note in `timeout.rs`. env's operand list ends with the utility
    // and its arguments, so a leading-hyphen token after the utility name must
    // be passed through rather than parsed as one of env's own options.
    #[arg(
        trailing_var_arg = true,
        allow_hyphen_values = true,
        help = gettext("NAME=VALUE pairs, the utility to invoke, and its arguments")
    )]
    operands: Vec<OsString>,
}

/// True if `name` is a valid environment variable name per the portable
/// character set: a non-digit `[A-Za-z_]` followed by `[A-Za-z0-9_]*`.
fn is_valid_name(name: &[u8]) -> bool {
    match name.split_first() {
        Some((&first, rest)) if first == b'_' || first.is_ascii_alphabetic() => {
            rest.iter().all(|&c| c == b'_' || c.is_ascii_alphanumeric())
        }
        _ => false,
    }
}

/// Split a `name=value` operand at its first '=', if the part before it is a
/// valid name. Bytes, not text: neither part need be valid UTF-8.
fn split_assignment(op: &OsStr) -> Option<(&OsStr, &OsStr)> {
    let bytes = op.as_bytes();
    let eq = bytes.iter().position(|&b| b == b'=')?;
    let (name, value) = (&bytes[..eq], &bytes[eq + 1..]);
    is_valid_name(name).then(|| (OsStr::from_bytes(name), OsStr::from_bytes(value)))
}

/// Split the operands into the leading assignments and the utility with its
/// arguments.
fn separate_ops(sv: &[OsString]) -> (&[OsString], &[OsString]) {
    let n_envs = sv
        .iter()
        .take_while(|s| split_assignment(s).is_some())
        .count();
    sv.split_at(n_envs)
}

fn merge_env(new_env: &[OsString], clear: bool) -> BTreeMap<OsString, OsString> {
    let mut map = BTreeMap::new();

    if !clear {
        // `vars_os`, not `vars`: an inherited entry need not be valid UTF-8,
        // and `vars` panics on one that is not.
        map.extend(env::vars_os());
    }

    for env_op in new_env {
        let (key, value) = split_assignment(env_op).expect("separate_ops checked it");
        map.insert(key.to_os_string(), value.to_os_string());
    }

    map
}

/// Write each `name=value` as its raw bytes, one per line.
fn print_env(envs: &BTreeMap<OsString, OsString>) -> io::Result<()> {
    // BTreeMap iterates in sorted key order, giving deterministic output.
    let mut out = BufWriter::new(io::stdout().lock());
    for (key, value) in envs {
        out.write_all(key.as_bytes())?;
        out.write_all(b"=")?;
        out.write_all(value.as_bytes())?;
        out.write_all(b"\n")?;
    }
    out.flush()
}

fn exec_util(envs: &BTreeMap<OsString, OsString>, util_args: &[OsString]) -> ! {
    let err = Command::new(&util_args[0])
        .args(&util_args[1..])
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .env_clear()
        .envs(envs)
        .exec();

    // exec() only returns on failure.
    exec_error_exit(&util_args[0].to_string_lossy(), err)
}

fn main() {
    diag::init_locale("env");

    let args = plib::optarg::parse::<Args>();

    let (envs, util_args) = separate_ops(&args.operands);
    let new_env = merge_env(envs, args.ignore_env);

    if util_args.is_empty() {
        if let Err(e) = print_env(&new_env) {
            diag::error(&format!("write error: {}", diag::io_error_text(&e)));
            std::process::exit(1);
        }
        return;
    }

    exec_util(&new_env, util_args);
}
