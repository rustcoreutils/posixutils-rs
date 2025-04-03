//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::HashMap;
use std::env;
use std::io;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};

#[derive(Parser)]
#[command(version, about = gettext("env - set the environment for command invocation"))]
struct Args {
    #[arg(
        short,
        long,
        help = gettext(
            "Invoke utility with exactly the environment specified by the arguments; the inherited environment shall be ignored completely"
        )
    )]
    ignore_env: bool,

    #[arg(help = gettext("NAME=VALUE pairs, the utility to invoke, and its arguments"))]
    operands: Vec<String>,
}

fn separate_ops(sv: &Vec<String>) -> (Vec<String>, Vec<String>) {
    let mut envs = Vec::new();
    let mut util_args = Vec::new();
    let mut in_envs = true;

    for s in sv {
        if in_envs {
            if s.contains('=') {
                envs.push(String::from(s));
                continue;
            }

            in_envs = false;

            // fall through
        }

        util_args.push(String::from(s));
    }

    (envs, util_args)
}

fn merge_env(new_env: &Vec<String>, clear: bool) -> HashMap<String, String> {
    let mut map = HashMap::new();

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

fn print_env(envs: HashMap<String, String>) -> Result<(), Box<dyn std::error::Error>> {
    for (key, value) in envs {
        println!("{}={}", key, value);
    }

    Ok(())
}

fn exec_util(envs: HashMap<String, String>, util_args: Vec<String>) -> io::Result<()> {
    Err(Command::new(&util_args[0])
        .args(&util_args[1..])
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .env_clear()
        .envs(&envs)
        .exec())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let (envs, util_args) = separate_ops(&args.operands);
    let new_env = merge_env(&envs, args.ignore_env);

    if util_args.is_empty() {
        return print_env(new_env);
    }

    exec_util(new_env, util_args)?;

    Ok(())
}
