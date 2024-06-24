//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::fs::File;
use std::io::{self, Write};
use std::process;

/// sh - shell, the standard command language interpreter
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Read commands from the standard input.
    #[arg(short, long)]
    stdin: bool,

    /// Read commands from the command_string operand.
    #[arg(short, long)]
    cmd: bool,

    /// Command files/arguments
    command_and_args: Vec<String>,
}

#[derive(Debug, PartialEq)]
enum Token {
    Generic(String),
    Operator(String),
    AndIf,     // &&
    OrIf,      // ||
    DSemi,     // ;;
    DLess,     // <<
    LessAnd,   // <&
    LessGreat, // <>
    DLessDash, // <<-
    DGreat,    // >>
    GreatAnd,  // >&
    Clobber,   // >|
    Semicolon,
    EndOfLine,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    let mut current_token = String::new();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' => {
                chars.next();
                if !current_token.is_empty() {
                    tokens.push(Token::Generic(current_token.clone()));
                    current_token.clear();
                }
            }
            '<' | '>' | '|' | '&' | ';' => {
                if !current_token.is_empty() {
                    tokens.push(Token::Generic(current_token.clone()));
                    current_token.clear();
                }
                let mut operator = String::new();
                operator.push(chars.next().unwrap());
                if let Some(&next_ch) = chars.peek() {
                    if operator == "<" && next_ch == '<' {
                        operator.push(chars.next().unwrap());
                        if let Some(&next_ch) = chars.peek() {
                            if next_ch == '-' {
                                operator.push(chars.next().unwrap());
                                tokens.push(Token::DLessDash);
                            } else {
                                tokens.push(Token::DLess);
                            }
                        } else {
                            tokens.push(Token::DLess);
                        }
                    } else if operator == "<" && next_ch == '&' {
                        operator.push(chars.next().unwrap());
                        tokens.push(Token::LessAnd);
                    } else if operator == "<" && next_ch == '>' {
                        operator.push(chars.next().unwrap());
                        tokens.push(Token::LessGreat);
                    } else if operator == ">" && next_ch == '>' {
                        operator.push(chars.next().unwrap());
                        tokens.push(Token::DGreat);
                    } else if operator == ">" && next_ch == '&' {
                        operator.push(chars.next().unwrap());
                        tokens.push(Token::GreatAnd);
                    } else if operator == ">" && next_ch == '|' {
                        operator.push(chars.next().unwrap());
                        tokens.push(Token::Clobber);
                    } else if operator == "|" && next_ch == '|' {
                        operator.push(chars.next().unwrap());
                        tokens.push(Token::OrIf);
                    } else if operator == "&" && next_ch == '&' {
                        operator.push(chars.next().unwrap());
                        tokens.push(Token::AndIf);
                    } else if operator == ";" {
                        if next_ch == ';' {
                            operator.push(chars.next().unwrap());
                            tokens.push(Token::DSemi);
                        } else {
                            tokens.push(Token::Semicolon);
                        }
                    } else {
                        tokens.push(Token::Operator(operator.clone()));
                    }
                } else {
                    if operator == ";" {
                        tokens.push(Token::Semicolon);
                    } else {
                        tokens.push(Token::Operator(operator.clone()));
                    }
                }
            }
            '#' => {
                if !current_token.is_empty() {
                    tokens.push(Token::Generic(current_token.clone()));
                    current_token.clear();
                }
                chars.next(); // Skip the '#'
                while let Some(&next_ch) = chars.peek() {
                    if next_ch == '\n' {
                        break;
                    }
                    chars.next();
                }
            }
            '\'' | '"' => {
                let quote = chars.next().unwrap();
                current_token.push(quote);
                while let Some(&next_ch) = chars.peek() {
                    current_token.push(chars.next().unwrap());
                    if next_ch == quote {
                        break;
                    }
                }
            }
            '$' | '`' => {
                current_token.push(chars.next().unwrap());
                while let Some(&next_ch) = chars.peek() {
                    current_token.push(chars.next().unwrap());
                    if next_ch == '`' || next_ch == ')' {
                        break;
                    }
                }
            }
            _ => {
                current_token.push(chars.next().unwrap());
            }
        }
    }

    if !current_token.is_empty() {
        tokens.push(Token::Generic(current_token));
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
    BuiltIn(BuiltInCommand),
    Sequential(Vec<Command>),
}

#[derive(Debug)]
enum BuiltInCommand {
    Cd(String),
    Exit,
}

fn parse(tokens: &[Token]) -> (Command, &[Token]) {
    parse_expr(tokens)
}

fn parse_expr(tokens: &[Token]) -> (Command, &[Token]) {
    let (mut left, mut tokens) = parse_pipe(tokens);
    let mut commands = vec![];

    while !tokens.is_empty() {
        match tokens[0] {
            Token::Semicolon => {
                commands.push(left); // Push the current command to the sequence
                let (right, new_tokens) = parse_pipe(&tokens[1..]);
                left = right;
                tokens = new_tokens;
            }
            Token::AndIf => {
                let (right, new_tokens) = parse_pipe(&tokens[1..]);
                left = Command::And(Box::new(left), Box::new(right));
                tokens = new_tokens;
            }
            Token::OrIf => {
                let (right, new_tokens) = parse_pipe(&tokens[1..]);
                left = Command::Or(Box::new(left), Box::new(right));
                tokens = new_tokens;
            }
            _ => break,
        }
    }

    commands.push(left); // Push the final command to the sequence

    if commands.len() > 1 {
        (Command::Sequential(commands), tokens)
    } else {
        (commands.pop().unwrap(), tokens)
    }
}

fn parse_pipe(tokens: &[Token]) -> (Command, &[Token]) {
    let (left, mut tokens) = parse_simple(tokens);
    let mut commands = vec![left];

    while !tokens.is_empty() {
        if let Token::Operator(ref op) = tokens[0] {
            if op == "|" {
                let (right, new_tokens) = parse_simple(&tokens[1..]);
                commands.push(right);
                tokens = new_tokens;
            } else {
                break;
            }
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
            Token::Generic(word) => {
                if word == "cd" {
                    if i + 1 < tokens.len() {
                        if let Token::Generic(dir) = &tokens[i + 1] {
                            return (
                                Command::BuiltIn(BuiltInCommand::Cd(dir.clone())),
                                &tokens[i + 2..],
                            );
                        }
                    }
                    return (
                        Command::BuiltIn(BuiltInCommand::Cd(String::new())),
                        &tokens[i + 1..],
                    );
                } else if word == "exit" {
                    return (Command::BuiltIn(BuiltInCommand::Exit), &tokens[i + 1..]);
                } else {
                    commands.push(word.clone());
                    i += 1;
                }
            }
            Token::DGreat => {
                if let Some(Token::Generic(file)) = tokens.get(i + 1) {
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
            Token::Operator(op) => {
                match op.as_str() {
                    "<" => {
                        if let Some(Token::Generic(file)) = tokens.get(i + 1) {
                            let (_, remaining_tokens) = parse_simple(&tokens[i + 2..]);
                            return (
                                Command::RedirectIn(
                                    Box::new(Command::Simple(commands)),
                                    file.clone(),
                                ),
                                remaining_tokens,
                            );
                        }
                    }
                    ">" => {
                        if let Some(Token::Generic(file)) = tokens.get(i + 1) {
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
                    ">>" => {
                        if let Some(Token::Generic(file)) = tokens.get(i + 1) {
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
                    "|" => {
                        return (Command::Simple(commands), &tokens[i..]);
                    }
                    _ => {
                        // Unsupported operator, handle appropriately
                        i += 1;
                    }
                }
            }
            _ => break,
        }
    }

    (Command::Simple(commands), &tokens[i..])
}

fn execute_builtin(builtin: BuiltInCommand) -> io::Result<()> {
    match builtin {
        BuiltInCommand::Cd(dir) => {
            let target_dir = if dir.is_empty() {
                std::env::var("HOME").unwrap()
            } else {
                dir
            };
            std::env::set_current_dir(target_dir)?;
            Ok(())
        }
        BuiltInCommand::Exit => {
            std::process::exit(0);
        }
    }
}

fn execute_simple(args: Vec<String>) -> io::Result<()> {
    let mut cmd = process::Command::new(&args[0]);
    if args.len() > 1 {
        cmd.args(&args[1..]);
    }
    let status = cmd.status()?;
    if !status.success() {
        return Err(io::Error::new(io::ErrorKind::Other, "command failed"));
    }
    Ok(())
}

fn execute_piped(commands: Vec<Command>) -> io::Result<()> {
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
    Ok(())
}

fn execute_and(left: Box<Command>, right: Box<Command>) -> io::Result<()> {
    if execute(*left).is_ok() {
        execute(*right)?;
    }
    Ok(())
}

fn execute_or(left: Box<Command>, right: Box<Command>) -> io::Result<()> {
    if execute(*left).is_err() {
        execute(*right)?;
    }
    Ok(())
}

fn execute_redirect_in(command: Box<Command>, file: String) -> io::Result<()> {
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
    Ok(())
}

fn execute_redirect_out(command: Box<Command>, file: String, append: bool) -> io::Result<()> {
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
    Ok(())
}

fn execute(command: Command) -> io::Result<()> {
    match command {
        Command::Simple(args) => execute_simple(args),
        Command::Piped(commands) => execute_piped(commands),
        Command::And(left, right) => execute_and(left, right),
        Command::Or(left, right) => execute_or(left, right),
        Command::RedirectIn(command, file) => execute_redirect_in(command, file),
        Command::RedirectOut(command, file, append) => execute_redirect_out(command, file, append),
        Command::BuiltIn(builtin) => execute_builtin(builtin),
        Command::Sequential(commands) => {
            for cmd in commands {
                execute(cmd)?;
            }
            Ok(())
        }
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
    // parse command line arguments
    let mut args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    // if no operands, and no -c, assume -s
    if !args.stdin && !args.cmd && args.command_and_args.is_empty() {
        args.stdin = true;
    }

    if args.cmd {
        let command = args.command_and_args.join(" ");
        let tokens = tokenize(&command);
        let (command, _) = parse(&tokens);
        execute(command)?;
        Ok(())
    } else {
        read_eval_print_loop()?;

        Ok(())
    }
}
