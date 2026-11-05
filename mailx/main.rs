//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod args;
mod commands;
mod compose;
mod error;
mod escapes;
mod filename;
mod lock;
mod mailbox;
mod mbox;
mod message;
mod msglist;
mod script;
mod send;
mod signals;
mod summary;
mod util;
mod variables;

use std::env;
use std::io::{self, IsTerminal, Write};
use std::process;

use gettextrs::gettext;

use args::{Args, Mode};
use commands::{execute_command, CommandResult, Interp};
use mailbox::Mailbox;
use variables::Variables;

fn main() {
    // Honor the environment locale (LC_CTYPE for multibyte text, LC_MESSAGES
    // for diagnostics); diagnostics are routed through gettext for
    // translatability. See the project locale-init convention.
    plib::diag::init_locale("mailx");

    // Install the SIGINT handler so an interrupt aborts the current command or
    // message instead of terminating mailx (ASYNCHRONOUS EVENTS).
    signals::setup();

    let args = match Args::parse(
        plib::optarg::args_utf8("mailx")
            .into_iter()
            .skip(1)
            .collect(),
    ) {
        Ok(args) => args,
        Err(e) => {
            eprintln!("mailx: {}", e);
            process::exit(1);
        }
    };

    let exit_code = match args.mode {
        Mode::Send => run_send_mode(&args),
        Mode::CheckMail => run_check_mail(&args),
        Mode::HeadersOnly => run_headers_only(&args),
        Mode::Receive => run_receive_mode(&args),
    };
    process::exit(exit_code);
}

fn run_send_mode(args: &Args) -> i32 {
    let (mut vars, mut interp) = start_up(args, true);
    exit_status(send::send_mode(args, &mut vars, &mut interp))
}

fn run_check_mail(args: &Args) -> i32 {
    match Mailbox::load(&named_or_system_mailbox(args)) {
        Ok(mb) if mb.message_count() > 0 => 0,
        _ => 1,
    }
}

fn run_headers_only(args: &Args) -> i32 {
    let (vars, _) = start_up(args, false);
    let mb = match open_mailbox(args, &vars) {
        Ok(mb) => mb,
        Err(e) => {
            eprintln!("mailx: {}", e);
            return 1;
        }
    };
    if mb.message_count() == 0 {
        print_no_mail();
    } else {
        summary::print_headers(&mb, None, &vars);
    }
    0
}

fn run_receive_mode(args: &Args) -> i32 {
    let (mut vars, mut interp) = start_up(args, false);
    let mut mb = match open_mailbox(args, &vars) {
        Ok(mb) => mb,
        Err(e) => {
            eprintln!("mailx: {}", e);
            return 1;
        }
    };

    let is_tty = io::stdin().is_terminal();
    if !vars.is_set("quiet") && is_tty {
        println!("mailx-rs version {}", env!("CARGO_PKG_VERSION"));
    }
    if !args.no_header_summary && vars.is_set("header") && mb.message_count() > 0 {
        summary::print_page(&mb, &vars);
    } else if mb.message_count() == 0 && is_tty {
        print_no_mail();
    }

    command_loop(&mut mb, &mut vars, &mut interp, is_tty)
}

/// Read and run commands until `quit`, `exit` or end of input.
fn command_loop(mb: &mut Mailbox, vars: &mut Variables, interp: &mut Interp, is_tty: bool) -> i32 {
    loop {
        if is_tty {
            if let Some(prompt) = vars.get("prompt") {
                print!("{}", prompt);
                let _ = io::stdout().flush();
            }
        }

        let line = match read_command() {
            Ok(Command::Line(line)) => line,
            // SIGINT in command mode aborts the partial command and re-prompts.
            Ok(Command::Interrupted) => {
                println!();
                continue;
            }
            Ok(Command::Eof) => return exit_status(mb.quit(vars)),
            Err(e) => {
                eprintln!(
                    "mailx: {}: {}",
                    gettext("read error"),
                    plib::diag::io_error_text(&e)
                );
                return 1;
            }
        };
        if line.is_empty() {
            continue;
        }

        match execute_command(&line, mb, vars, interp) {
            Ok(CommandResult::Continue) => {}
            Ok(CommandResult::Quit) => return exit_status(mb.quit(vars)),
            Ok(CommandResult::Exit) => return 0,
            Err(e) => eprintln!("{}", e),
        }
    }
}

/// One line of command-mode input.
enum Command {
    Line(String),
    Interrupted,
    Eof,
}

fn read_command() -> io::Result<Command> {
    let mut line = String::new();
    match signals::read_line_interruptible(&mut line) {
        Ok(0) => return Ok(Command::Eof),
        Ok(_) => {}
        Err(e) if e.kind() == io::ErrorKind::Interrupted => {
            signals::take_sigint();
            return Ok(Command::Interrupted);
        }
        Err(e) => return Err(e),
    }
    // A SIGINT may have arrived between reads; it aborts this command too.
    if signals::take_sigint() {
        return Ok(Command::Interrupted);
    }
    Ok(Command::Line(line.trim().to_string()))
}

/// The exit status for the outcome of a mode, its error reported.
fn exit_status(result: error::Result<()>) -> i32 {
    match result {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("mailx: {}", e);
            1
        }
    }
}

fn print_no_mail() {
    println!(
        "{} {}",
        gettext("No mail for"),
        util::user_login_or_unknown()
    );
}

/// Variables and interpreter state after the Start-Up steps: defaults,
/// imported environment variables, options, then the start-up files.
fn start_up(args: &Args, send_mode: bool) -> (Variables, Interp) {
    let mut vars = Variables::new();
    for name in [
        "DEAD", "EDITOR", "MBOX", "LISTER", "PAGER", "SHELL", "VISUAL",
    ] {
        if let Ok(value) = env::var(name) {
            vars.set(name, &value);
        }
    }
    if args.ignore_interrupts {
        vars.enable("ignore");
    }

    let mut interp = Interp {
        send_mode,
        ..Interp::default()
    };
    load_startup_files(&mut vars, &mut interp, args.no_init);
    (vars, interp)
}

/// System startup file locations (checked in order)
const SYSTEM_MAILRC_PATHS: &[&str] = &[
    "/etc/mail.rc",
    "/etc/mailrc",
    "/usr/share/mailx/mailx.rc",
    "/usr/lib/mailx/mailx.rc",
];

/// Run the system start-up file, unless `-n`, then the user's `MAILRC`.
///
/// `-n` attaches only to the system file; the user's is always read.
fn load_startup_files(vars: &mut Variables, interp: &mut Interp, no_init: bool) {
    if !no_init {
        if let Some((path, content)) = SYSTEM_MAILRC_PATHS
            .iter()
            .find_map(|p| std::fs::read_to_string(p).ok().map(|c| (p, c)))
        {
            run_startup_file(&content, path, vars, interp);
        }
    }

    let mailrc = env::var("MAILRC").unwrap_or_else(|_| format!("{}/.mailrc", util::home()));
    if let Ok(content) = std::fs::read_to_string(&mailrc) {
        run_startup_file(&content, &mailrc, vars, interp);
    }
}

fn run_startup_file(content: &str, path: &str, vars: &mut Variables, interp: &mut Interp) {
    // Start-up files have no message store. Commands that need one are excluded
    // by their context mask, so this scratch mailbox is never read from.
    let mut scratch = Mailbox::new(String::new());
    script::run_script(content, path, &mut scratch, vars, interp);
}

/// The mailbox to read: the `-u` user's system mailbox, the `-f` file, mbox
/// for a bare `-f`, or the user's own system mailbox.
fn open_mailbox(args: &Args, vars: &Variables) -> error::Result<Mailbox> {
    if args.file.is_none() && args.user.is_none() && args.read_mbox {
        // mbox need not exist yet.
        let path = util::mbox_path(vars);
        return Ok(Mailbox::load(&path).unwrap_or_else(|_| Mailbox::new(path)));
    }
    let mut mb = Mailbox::load(&named_or_system_mailbox(args))?;
    mb.is_system_mailbox = args.file.is_none() && args.user.is_none();
    Ok(mb)
}

/// The `-u` user's system mailbox, else the `-f` file, else the user's own
/// system mailbox.
fn named_or_system_mailbox(args: &Args) -> String {
    if let Some(user) = &args.user {
        // Read access is governed by the file's permissions, which provide
        // the "appropriate privileges" check -u requires.
        return util::spool_path(user);
    }
    if let Some(file) = &args.file {
        return file.clone();
    }
    util::system_mailbox()
}
