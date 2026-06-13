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
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use posixutils_i18n::gettext_lib::lookup::{expand_escapes, MessageLookup};
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

    #[arg(short = 'e', help = gettext("Process C-language escape sequences in MSGID"))]
    expand_escapes: bool,

    #[arg(short = 'E', conflicts_with = "expand_escapes", help = gettext("Do not process C-language escape sequences in MSGID"))]
    no_expand_escapes: bool,

    #[arg(short = 'n', help = gettext("Do not append a trailing newline (with -s)"))]
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

    // Non-`-s` form: `[textdomain] msgid` (exactly one msgid).
    let (operand_domain, msgid): (Option<&str>, &str) = match args.args.len() {
        0 => {
            eprintln!("gettext: missing message");
            exit(1);
        }
        1 => (None, &args.args[0]),
        2 => (Some(args.args[0].as_str()), &args.args[1]),
        _ => {
            eprintln!("gettext: too many arguments");
            exit(1);
        }
    };

    let domain = resolve_domain(operand_domain, &args.domain);
    // Escapes (if any) are applied to the operand, which is both the lookup key
    // and the fallback when the catalog has no entry.
    let key = if args.expand_escapes {
        expand_escapes(msgid)
    } else {
        msgid.to_string()
    };

    // A NULL text domain (none of operand/-d/TEXTDOMAIN) returns the msgid
    // directly, with no catalog lookup.
    let output = match domain {
        Some(domain) => MessageLookup::new()
            .gettext(&domain, &key)
            .unwrap_or_else(|| key.clone()),
        None => key,
    };

    // The non-`-s` form does not append a trailing newline.
    print!("{}", output);
}

/// Resolve the text domain, in decreasing precedence: the operand `textdomain`,
/// the `-d` option, then the `TEXTDOMAIN` environment variable. Returns `None`
/// (a NULL text domain) when none is available.
fn resolve_domain(operand: Option<&str>, opt_domain: &Option<String>) -> Option<String> {
    if let Some(d) = operand {
        if !d.is_empty() {
            return Some(d.to_string());
        }
    }
    if let Some(d) = opt_domain {
        if !d.is_empty() {
            return Some(d.clone());
        }
    }
    std::env::var("TEXTDOMAIN").ok().filter(|d| !d.is_empty())
}

/// Shell mode (`-s`): translate each msgid operand and write them separated by a
/// single space, appending a newline unless `-n` is given.
fn shell_mode(args: &Args) {
    let domain = resolve_domain(None, &args.domain);
    let mut lookup = MessageLookup::new();

    for (i, msgid) in args.args.iter().enumerate() {
        // With -s the default is -E (no escape processing) unless -e is given.
        let key = if args.expand_escapes {
            expand_escapes(msgid)
        } else {
            msgid.clone()
        };
        let output = match &domain {
            Some(domain) => lookup.gettext(domain, &key).unwrap_or_else(|| key.clone()),
            None => key,
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
