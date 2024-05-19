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
use std::fs::File;
use std::io::{self, Write};
use std::process::{self, Child};

enum Input {
    ChangeDir(String),
    Exec(Vec<Vec<String>>, Option<String>, Option<(String, bool)>), // Commands, stdin file, stdout file (and append flag)
    Exit,
    NoOp,
}

fn exec_piped_commands(
    commands: Vec<Vec<String>>,
    stdin: Option<String>,
    stdout: Option<(String, bool)>,
) -> io::Result<()> {
    let mut previous_command: Option<Child> = None;

    for (i, command) in commands.iter().enumerate() {
        let (input, args) = command.split_first().expect("command is empty");
        let mut cmd = process::Command::new(input);
        cmd.args(args);

        if let Some(ref input_file) = stdin {
            if i == 0 {
                cmd.stdin(process::Stdio::from(File::open(input_file)?));
            }
        } else if let Some(previous) = previous_command {
            cmd.stdin(process::Stdio::from(previous.stdout.unwrap()));
        } else {
            cmd.stdin(process::Stdio::inherit());
        }

        if i < commands.len() - 1 {
            cmd.stdout(process::Stdio::piped());
        } else {
            if let Some((ref output_file, append)) = stdout {
                if append {
                    cmd.stdout(process::Stdio::from(
                        File::options().append(true).open(output_file)?,
                    ));
                } else {
                    cmd.stdout(process::Stdio::from(File::create(output_file)?));
                }
            } else {
                cmd.stdout(process::Stdio::inherit());
            }
        }

        previous_command = Some(cmd.spawn()?);
    }

    if let Some(mut final_command) = previous_command {
        final_command.wait()?;
    }

    Ok(())
}

fn parse_input(rawline: &str) -> Input {
    let mut commands = Vec::new();
    let mut stdin = None;
    let mut stdout = None;
    let mut current_command = Vec::new();

    let parts: Vec<&str> = rawline.split_whitespace().collect();
    let mut iter = parts.into_iter().peekable();

    while let Some(part) = iter.next() {
        match part {
            "|" => {
                commands.push(current_command);
                current_command = Vec::new();
            }
            "<" => {
                stdin = iter.next().map(String::from);
            }
            ">" => {
                stdout = iter.next().map(|s| (s.to_string(), false));
            }
            ">>" => {
                stdout = iter.next().map(|s| (s.to_string(), true));
            }
            _ => {
                current_command.push(part.to_string());
            }
        }
    }

    if !current_command.is_empty() {
        commands.push(current_command);
    }

    if commands.is_empty() {
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
        _ => Input::Exec(commands, stdin, stdout),
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
        Input::Exec(commands, stdin, stdout) => {
            exec_piped_commands(commands, stdin, stdout)?;
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
