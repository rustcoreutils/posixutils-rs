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
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use posixutils_i18n::gettext_lib::lookup::{MessageLookup, expand_escapes};
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

    #[arg(short = 'e', help = gettext("Enable interpretation of some escape sequences"))]
    expand_escapes: bool,

    #[arg(short, long, action = clap::ArgAction::HelpLong, help = gettext("Print help"))]
    help: Option<bool>,

    #[arg(short = 'V', long, action = clap::ArgAction::Version, help = gettext("Print version"))]
    version: Option<bool>,

    #[arg(help = gettext("Singular form (MSGID1)"))]
    msgid1: String,

    #[arg(help = gettext("Plural form (MSGID2)"))]
    msgid2: String,

    #[arg(help = gettext("Count for plural selection"))]
    count: String,
}

fn main() {
    // Set up localization
    setlocale(LocaleCategory::LcAll, "");
    if textdomain("posixutils-rs").is_err() {
        // Ignore error - translation may not be available
    }
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");

    let args = Args::parse();

    // Parse the count
    let count: u64 = match args.count.parse() {
        Ok(n) => n,
        Err(_) => {
            eprintln!("ngettext: invalid count: {}", args.count);
            exit(1);
        }
    };

    // Get the domain
    let domain = args
        .domain
        .clone()
        .or_else(|| std::env::var("TEXTDOMAIN").ok())
        .unwrap_or_else(|| "messages".to_string());

    // Look up the message
    let mut lookup = MessageLookup::new();

    let translated = lookup
        .ngettext(&domain, &args.msgid1, &args.msgid2, count)
        .map(|s| s.to_string())
        .unwrap_or_else(|| {
            // Fallback: use simple Germanic plural rule
            if count == 1 {
                args.msgid1.clone()
            } else {
                args.msgid2.clone()
            }
        });

    let output = if args.expand_escapes {
        expand_escapes(&translated)
    } else {
        translated
    };

    println!("{}", output);
}
