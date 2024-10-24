// SPDX-License-Identifier: MIT

use clap::{
    error::{ContextKind, ContextValue, ErrorKind},
    Parser,
};
use gettextrs::gettext;
use std::process::exit;

pub trait ClapLocale: Parser {
    fn parse_with_locale() -> Self {
        Self::try_parse().unwrap_or_else(|err| {
            match err.kind() {
                ErrorKind::DisplayHelp | ErrorKind::DisplayVersion => {
                    eprint!("{}", err);
                    exit(0);
                }
                ErrorKind::MissingRequiredArgument => {
                    eprintln!(
                        "{}",
                        gettext("error: the following required arguments were not provided:")
                    );
                    for (k, v) in err.context() {
                        match k {
                            ContextKind::InvalidArg => {
                                if let ContextValue::Strings(v) = v {
                                    for i in v {
                                        eprintln!("  {i}");
                                    }
                                }
                            }
                            ContextKind::Usage => {} // ignore
                            _ => unreachable!(),
                        }
                    }
                }
                ErrorKind::UnknownArgument => {
                    for (k, v) in err.context() {
                        match k {
                            ContextKind::InvalidArg => {
                                if let ContextValue::String(v) = v {
                                    eprintln!(
                                        "{}",
                                        gettext!("error: unexpected argument '{}' found", v)
                                    );
                                }
                            }
                            ContextKind::Usage => {}     // ignore
                            ContextKind::Suggested => {} // ignore
                            _ => unreachable!(),
                        }
                    }
                }
                _ => {
                    eprint!("{}", err);
                    exit(0);
                }
            };
            eprintln!();
            eprintln!("{}", gettext("For more information, try '--help'."));
            exit(0);
        })
    }
}
