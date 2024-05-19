//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate plib;

use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::io::{self, Write};
use std::process::{self, Child};

enum Input {
    ChangeDir(String),
    Exec(Vec<Vec<String>>), // List of commands for piping
    Exit,
    NoOp,
}

fn exec_piped_commands(commands: Vec<Vec<String>>) -> io::Result<()> {
    let mut previous_command: Option<Child> = None;

    for (i, command) in commands.iter().enumerate() {
        let (input, args) = command.split_first().expect("command is empty");
        let mut cmd = process::Command::new(input);
        cmd.args(args);

        if let Some(previous) = previous_command {
            cmd.stdin(process::Stdio::from(previous.stdout.unwrap()));
        } else {
            cmd.stdin(process::Stdio::inherit());
        }

        if i < commands.len() - 1 {
            cmd.stdout(process::Stdio::piped());
        } else {
            cmd.stdout(process::Stdio::inherit());
        }

        previous_command = Some(cmd.spawn()?);
    }

    if let Some(mut final_command) = previous_command {
        final_command.wait()?;
    }

    Ok(())
}

fn parse_input(rawline: &str) -> Input {
    let commands: Vec<Vec<String>> = rawline
        .split('|')
        .map(|cmd| cmd.split_whitespace().map(|s| s.to_string()).collect())
        .collect();

    if commands.is_empty() {
        return Input::NoOp;
    }

    if commands[0].is_empty() {
        return Input::NoOp;
    }

    match commands[0][0].as_str() {
        "cd" => {
            if commands[0].len() == 1 {
                Input::ChangeDir("".to_string())
            } else {
                Input::ChangeDir(commands[0][1].clone())
            }
        }
        "exit" => Input::Exit,
        _ => Input::Exec(commands),
    }
}

fn read_eval_print() -> io::Result<bool> {
    // display prompt
    print!("$ ");
    io::stdout().flush()?;

    // read a line of shell input
    let mut rawline = String::new();
    let n_read = io::stdin().read_line(&mut rawline)?;
    if n_read == 0 {
        return Ok(false);
    }

    // parse the input
    let input = parse_input(&rawline);

    // execute based on input
    match input {
        Input::ChangeDir(dir) => {
            if dir.is_empty() {
                let home = std::env::var("HOME").unwrap();
                std::env::set_current_dir(home)?;
            } else {
                std::env::set_current_dir(dir)?;
            }
        }
        Input::Exit => return Ok(false),
        Input::Exec(commands) => {
            exec_piped_commands(commands)?;
        }
        Input::NoOp => {}
    }
    Ok(true)
}

fn read_eval_print_loop() -> io::Result<()> {
    loop {
        match read_eval_print() {
            Ok(false) => break,
            Err(e) => eprintln!("Error: {}", e),
            _ => {}
        }
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    read_eval_print_loop()?;

    Ok(())
}
