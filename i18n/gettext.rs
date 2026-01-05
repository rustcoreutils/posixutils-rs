//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! gettext - retrieve text string from message database
//!
//! The gettext utility retrieves a translated text string corresponding
//! to a given msgid from a message catalog for the current locale.

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use posixutils_i18n::gettext_lib::lookup::{MessageLookup, expand_escapes};
use std::process::exit;

/// gettext - retrieve text string from message database
#[derive(Parser)]
#[command(
    version,
    about = gettext("gettext - retrieve text string from message database"),
    disable_help_flag = true,
    disable_version_flag = true
)]
struct Args {
    #[arg(short = 'd', long = "domain", help = gettext("Use TEXTDOMAIN as the text domain for translating MSGID"))]
    domain: Option<String>,

    #[arg(short = 'e', help = gettext("Enable interpretation of some escape sequences"))]
    expand_escapes: bool,

    #[arg(short = 'n', help = gettext("Suppress trailing newline"))]
    no_newline: bool,

    #[arg(short = 's', help = gettext("Behave like echo command (interpret multiple MSGIDs)"))]
    shell_mode: bool,

    #[arg(short, long, action = clap::ArgAction::HelpLong, help = gettext("Print help"))]
    help: Option<bool>,

    #[arg(short = 'V', long, action = clap::ArgAction::Version, help = gettext("Print version"))]
    version: Option<bool>,

    #[arg(trailing_var_arg = true, help = gettext("TEXTDOMAIN and/or MSGID. If one argument: MSGID. If two arguments: TEXTDOMAIN MSGID"))]
    args: Vec<String>,
}

fn main() {
    // Set up localization
    setlocale(LocaleCategory::LcAll, "");
    if textdomain("posixutils-rs").is_err() {
        // Ignore error - translation may not be available
    }
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");

    let args = Args::parse();

    // Handle shell mode (-s)
    if args.shell_mode {
        shell_mode(&args);
        return;
    }

    // Parse positional arguments to determine domain and msgid
    let (domain, msgids) = match args.args.len() {
        0 => {
            eprintln!("gettext: missing arguments");
            exit(1);
        }
        1 => {
            // Single argument: use TEXTDOMAIN env var or -d option
            let domain = args
                .domain
                .clone()
                .or_else(|| std::env::var("TEXTDOMAIN").ok())
                .unwrap_or_else(|| "messages".to_string());
            (domain, vec![args.args[0].clone()])
        }
        _ => {
            // Two or more arguments: first is domain, rest are msgids
            // Unless -d was specified, then all are msgids
            if args.domain.is_some() {
                (args.domain.clone().unwrap(), args.args.clone())
            } else {
                (args.args[0].clone(), args.args[1..].to_vec())
            }
        }
    };

    // Look up and print the message(s)
    let mut lookup = MessageLookup::new();

    for (i, msgid) in msgids.iter().enumerate() {
        let translated = lookup
            .gettext(&domain, msgid)
            .map(|s| s.to_string())
            .unwrap_or_else(|| msgid.clone());

        let output = if args.expand_escapes {
            expand_escapes(&translated)
        } else {
            translated
        };

        if i > 0 {
            print!(" ");
        }
        print!("{}", output);
    }

    if !args.no_newline {
        println!();
    }
}

/// Shell mode: behave like echo, translating each argument
fn shell_mode(args: &Args) {
    let domain = args
        .domain
        .clone()
        .or_else(|| std::env::var("TEXTDOMAIN").ok())
        .unwrap_or_else(|| "messages".to_string());

    let mut lookup = MessageLookup::new();

    for (i, msgid) in args.args.iter().enumerate() {
        let translated = lookup
            .gettext(&domain, msgid)
            .map(|s| s.to_string())
            .unwrap_or_else(|| msgid.clone());

        let output = if args.expand_escapes {
            expand_escapes(&translated)
        } else {
            translated
        };

        if i > 0 {
            print!(" ");
        }
        print!("{}", output);
    }

    if !args.no_newline {
        println!();
    }
}
