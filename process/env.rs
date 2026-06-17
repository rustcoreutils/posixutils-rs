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

    #[arg(help = gettext("NAME=VALUE pairs, the utility to invoke, and its arguments"))]
    operands: Vec<String>,
}

/// True if `name` is a valid environment variable name per the portable
/// character set: a non-digit `[A-Za-z_]` followed by `[A-Za-z0-9_]*`.
fn is_valid_name(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c == '_' || c.is_ascii_alphabetic() => {}
        _ => return false,
    }
    chars.all(|c| c == '_' || c.is_ascii_alphanumeric())
}

fn separate_ops(sv: &[String]) -> (Vec<String>, Vec<String>) {
    // Upper bound: all operands could be envs or all could be args
    let mut envs = Vec::with_capacity(sv.len());
    let mut util_args = Vec::with_capacity(sv.len());
    let mut in_envs = true;

    for s in sv {
        if in_envs {
            // A leading `name=value` operand is an assignment only when the
            // part before the first '=' is a valid environment-variable name.
            if let Some((name, _)) = s.split_once('=') {
                if is_valid_name(name) {
                    envs.push(String::from(s));
                    continue;
                }
            }

            in_envs = false;

            // fall through
        }

        util_args.push(String::from(s));
    }

    (envs, util_args)
}

fn merge_env(new_env: &[String], clear: bool) -> BTreeMap<String, String> {
    let mut map = BTreeMap::new();

    if !clear {
        for (key, value) in env::vars() {
            map.insert(key, value);
        }
    }

    for env_op in new_env {
        let (key, value) = env_op.split_once('=').unwrap();
        map.insert(String::from(key), String::from(value));
    }

    map
}

fn print_env(envs: &BTreeMap<String, String>) {
    // BTreeMap iterates in sorted key order, giving deterministic output.
    for (key, value) in envs {
        println!("{}={}", key, value);
    }
}

fn exec_util(envs: &BTreeMap<String, String>, util_args: &[String]) -> ! {
    let err = Command::new(&util_args[0])
        .args(&util_args[1..])
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .env_clear()
        .envs(envs)
        .exec();

    // exec() only returns on failure.
    exec_error_exit(&util_args[0], err)
}

fn main() {
    diag::init_locale("env");

    let args = Args::parse();

    let (envs, util_args) = separate_ops(&args.operands);
    let new_env = merge_env(&envs, args.ignore_env);

    if util_args.is_empty() {
        print_env(&new_env);
        return;
    }

    exec_util(&new_env, &util_args);
}
