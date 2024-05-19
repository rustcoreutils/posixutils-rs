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
use std::process;

#[derive(Debug, PartialEq)]
enum Token {
    Word(String),
    Pipe,
    RedirectIn,
    RedirectOut,
    RedirectAppend,
    And,
    Or,
    EndOfLine,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' => {
                chars.next();
            }
            '<' => {
                chars.next();
                tokens.push(Token::RedirectIn);
            }
            '>' => {
                chars.next();
                if chars.peek() == Some(&'>') {
                    chars.next();
                    tokens.push(Token::RedirectAppend);
                } else {
                    tokens.push(Token::RedirectOut);
                }
            }
            '&' => {
                chars.next();
                if chars.peek() == Some(&'&') {
                    chars.next();
                    tokens.push(Token::And);
                }
            }
            '|' => {
                chars.next();
                if chars.peek() == Some(&'|') {
                    chars.next();
                    tokens.push(Token::Or);
                } else {
                    tokens.push(Token::Pipe);
                }
            }
            _ => {
                let mut word = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch == ' ' || ch == '|' || ch == '&' || ch == '<' || ch == '>' {
                        break;
                    }
                    word.push(ch);
                    chars.next();
                }
                tokens.push(Token::Word(word));
            }
        }
    }

    tokens.push(Token::EndOfLine);
    tokens
}

#[derive(Debug)]
enum Command {
    Simple(Vec<String>),
    Piped(Vec<Command>),
    And(Box<Command>, Box<Command>),
    Or(Box<Command>, Box<Command>),
    RedirectIn(Box<Command>, String),
    RedirectOut(Box<Command>, String, bool), // bool for append
}

fn parse(tokens: &[Token]) -> (Command, &[Token]) {
    parse_expr(tokens)
}

fn parse_expr(tokens: &[Token]) -> (Command, &[Token]) {
    let (mut left, mut tokens) = parse_pipe(tokens);

    while !tokens.is_empty() {
        match tokens[0] {
            Token::And => {
                let (right, new_tokens) = parse_pipe(&tokens[1..]);
                left = Command::And(Box::new(left), Box::new(right));
                tokens = new_tokens;
            }
            Token::Or => {
                let (right, new_tokens) = parse_pipe(&tokens[1..]);
                left = Command::Or(Box::new(left), Box::new(right));
                tokens = new_tokens;
            }
            _ => break,
        }
    }

    (left, tokens)
}

fn parse_pipe(tokens: &[Token]) -> (Command, &[Token]) {
    let (left, mut tokens) = parse_simple(tokens);
    let mut commands = vec![left];

    while !tokens.is_empty() {
        if tokens[0] == Token::Pipe {
            let (right, new_tokens) = parse_simple(&tokens[1..]);
            commands.push(right);
            tokens = new_tokens;
        } else {
            break;
        }
    }

    if commands.len() == 1 {
        (commands.pop().unwrap(), tokens)
    } else {
        (Command::Piped(commands), tokens)
    }
}

fn parse_simple(tokens: &[Token]) -> (Command, &[Token]) {
    let mut commands = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        match &tokens[i] {
            Token::Word(word) => {
                commands.push(word.clone());
                i += 1;
            }
            Token::RedirectIn => {
                if let Token::Word(file) = &tokens[i + 1] {
                    let (_, remaining_tokens) = parse_simple(&tokens[i + 2..]);
                    return (
                        Command::RedirectIn(Box::new(Command::Simple(commands)), file.clone()),
                        remaining_tokens,
                    );
                }
            }
            Token::RedirectOut => {
                if let Token::Word(file) = &tokens[i + 1] {
                    let (_, remaining_tokens) = parse_simple(&tokens[i + 2..]);
                    return (
                        Command::RedirectOut(
                            Box::new(Command::Simple(commands)),
                            file.clone(),
                            false,
                        ),
                        remaining_tokens,
                    );
                }
            }
            Token::RedirectAppend => {
                if let Token::Word(file) = &tokens[i + 1] {
                    let (_, remaining_tokens) = parse_simple(&tokens[i + 2..]);
                    return (
                        Command::RedirectOut(
                            Box::new(Command::Simple(commands)),
                            file.clone(),
                            true,
                        ),
                        remaining_tokens,
                    );
                }
            }
            _ => break,
        }
    }

    (Command::Simple(commands), &tokens[i..])
}

fn execute(command: Command) -> io::Result<()> {
    match command {
        Command::Simple(args) => {
            let mut cmd = process::Command::new(&args[0]);
            if args.len() > 1 {
                cmd.args(&args[1..]);
            }
            let status = cmd.status()?;
            if !status.success() {
                return Err(io::Error::new(io::ErrorKind::Other, "command failed"));
            }
        }
        Command::Piped(commands) => {
            let mut previous_command: Option<process::Child> = None;
            let mut iter = commands.iter().peekable();

            while let Some(command) = iter.next() {
                let mut cmd = match command {
                    Command::Simple(args) => {
                        let mut cmd = process::Command::new(&args[0]);
                        if args.len() > 1 {
                            cmd.args(&args[1..]);
                        }
                        cmd
                    }
                    Command::RedirectIn(sub_command, file) => {
                        let mut cmd = match **sub_command {
                            Command::Simple(ref args) => {
                                let mut cmd = process::Command::new(&args[0]);
                                if args.len() > 1 {
                                    cmd.args(&args[1..]);
                                }
                                cmd
                            }
                            _ => unimplemented!(),
                        };
                        cmd.stdin(process::Stdio::from(File::open(file)?));
                        cmd
                    }
                    Command::RedirectOut(sub_command, file, append) => {
                        let mut cmd = match **sub_command {
                            Command::Simple(ref args) => {
                                let mut cmd = process::Command::new(&args[0]);
                                if args.len() > 1 {
                                    cmd.args(&args[1..]);
                                }
                                cmd
                            }
                            _ => unimplemented!(),
                        };
                        let file = if *append {
                            File::options().create(true).append(true).open(file)?
                        } else {
                            File::create(file)?
                        };
                        cmd.stdout(process::Stdio::from(file));
                        cmd
                    }
                    _ => unimplemented!(),
                };

                if let Some(previous) = previous_command {
                    cmd.stdin(previous.stdout.unwrap());
                } else {
                    cmd.stdin(process::Stdio::inherit());
                }

                if iter.peek().is_some() {
                    cmd.stdout(process::Stdio::piped());
                } else {
                    cmd.stdout(process::Stdio::inherit());
                }

                previous_command = Some(cmd.spawn()?);
            }

            if let Some(mut final_command) = previous_command {
                final_command.wait()?;
            }
        }
        Command::And(left, right) => {
            if execute(*left).is_ok() {
                execute(*right)?;
            }
        }
        Command::Or(left, right) => {
            if execute(*left).is_err() {
                execute(*right)?;
            }
        }
        Command::RedirectIn(command, file) => {
            let mut cmd = match *command {
                Command::Simple(ref args) => {
                    let mut cmd = process::Command::new(&args[0]);
                    if args.len() > 1 {
                        cmd.args(&args[1..]);
                    }
                    cmd
                }
                _ => unimplemented!(),
            };

            cmd.stdin(process::Stdio::from(File::open(file)?));
            let status = cmd.status()?;
            if !status.success() {
                return Err(io::Error::new(io::ErrorKind::Other, "command failed"));
            }
        }
        Command::RedirectOut(command, file, append) => {
            let mut cmd = match *command {
                Command::Simple(ref args) => {
                    let mut cmd = process::Command::new(&args[0]);
                    if args.len() > 1 {
                        cmd.args(&args[1..]);
                    }
                    cmd
                }
                _ => unimplemented!(),
            };

            let file = if append {
                File::options().create(true).append(true).open(file)?
            } else {
                File::create(file)?
            };
            cmd.stdout(process::Stdio::from(file));
            let status = cmd.status()?;
            if !status.success() {
                return Err(io::Error::new(io::ErrorKind::Other, "command failed"));
            }
        }
    }
    Ok(())
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

    // tokenize the input
    let tokens = tokenize(&rawline.trim());

    // println!("Tokens: {:?}", tokens);

    // parse the tokens into a command
    let (command, _) = parse(&tokens);

    // execute the command
    if let Err(e) = execute(command) {
        eprintln!("Error: {}", e);
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
