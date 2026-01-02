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
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use posixutils_i18n::gettext_lib::lookup::{expand_escapes, MessageLookup};
use std::process::exit;

/// ngettext - translate message and choose plural form
#[derive(Parser)]
#[command(
    version,
    about = "ngettext - translate message and choose plural form",
    disable_help_flag = true,
    disable_version_flag = true
)]
struct Args {
    /// Use TEXTDOMAIN as the text domain for translating MSGID
    #[arg(short = 'd', long = "domain")]
    domain: Option<String>,

    /// Enable interpretation of some escape sequences
    #[arg(short = 'e')]
    expand_escapes: bool,

    /// Print help
    #[arg(short, long, action = clap::ArgAction::HelpLong)]
    help: Option<bool>,

    /// Print version
    #[arg(short = 'V', long, action = clap::ArgAction::Version)]
    version: Option<bool>,

    /// Singular form (MSGID1)
    msgid1: String,

    /// Plural form (MSGID2)
    msgid2: String,

    /// Count for plural selection
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
