//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! ngettext - translate message and choose plural form
//!
//! The ngettext utility retrieves a translated text string corresponding
//! to a given msgid from a message catalog for the current locale,
//! choosing the appropriate plural form based on a count value.

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use posixutils_i18n::gettext_lib::lookup::{expand_escapes, MessageLookup};
use std::process::exit;

/// ngettext - translate message and choose plural form
#[derive(Parser)]
#[command(
    version,
    about = gettext("ngettext - translate message and choose plural form"),
    disable_help_flag = true,
    disable_version_flag = true
)]
struct Args {
    #[arg(short = 'd', long = "domain", help = gettext("Use TEXTDOMAIN as the text domain for translating MSGID"))]
    domain: Option<String>,

    #[arg(short = 'e', help = gettext("Process C-language escape sequences in MSGID operands"))]
    expand_escapes: bool,

    #[arg(short = 'E', conflicts_with = "expand_escapes", help = gettext("Do not process C-language escape sequences in MSGID operands"))]
    no_expand_escapes: bool,

    #[arg(short, long, action = clap::ArgAction::HelpLong, help = gettext("Print help"))]
    help: Option<bool>,

    #[arg(short = 'V', long, action = clap::ArgAction::Version, help = gettext("Print version"))]
    version: Option<bool>,

    #[arg(help = gettext("[textdomain] msgid msgid_plural n"))]
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

    // Operands: `[textdomain] msgid msgid_plural n`.
    let (operand_domain, msgid1, msgid2, count_str): (Option<&str>, &str, &str, &str) = match args
        .args
        .len()
    {
        3 => (None, &args.args[0], &args.args[1], &args.args[2]),
        4 => (
            Some(args.args[0].as_str()),
            &args.args[1],
            &args.args[2],
            &args.args[3],
        ),
        _ => {
            eprintln!("ngettext: usage: ngettext [-e|-E] [-d textdomain] [textdomain] msgid msgid_plural n");
            exit(1);
        }
    };

    // Parse the count (POSIX: as if by strtoul, base 10).
    let count: u64 = match count_str.parse() {
        Ok(n) => n,
        Err(_) => {
            eprintln!("ngettext: invalid count: {}", count_str);
            exit(1);
        }
    };

    let domain = resolve_domain(operand_domain, &args.domain);

    // Escapes (if any) apply to the msgid operands.
    let key1 = if args.expand_escapes {
        expand_escapes(msgid1)
    } else {
        msgid1.to_string()
    };
    let key2 = if args.expand_escapes {
        expand_escapes(msgid2)
    } else {
        msgid2.to_string()
    };

    // Germanic default used both as the catalog-miss fallback and when the text
    // domain is NULL (no catalog lookup).
    let germanic = || {
        if count == 1 {
            key1.clone()
        } else {
            key2.clone()
        }
    };

    let output = match domain {
        Some(domain) => MessageLookup::new()
            .ngettext(&domain, &key1, &key2, count)
            .unwrap_or_else(germanic),
        None => germanic(),
    };

    // No trailing newline is appended.
    print!("{}", output);
}

/// Resolve the text domain in decreasing precedence: operand `textdomain`, the
/// `-d` option, then `TEXTDOMAIN`. Returns `None` for a NULL text domain.
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
