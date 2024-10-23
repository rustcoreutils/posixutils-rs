//
// Copyright (c) 2024 fox0
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::HashSet;
use std::ffi::OsStr;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::process::exit;

use clap::Parser;
#[cfg(debug_assertions)]
use gettextrs::bindtextdomain;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use proc_macro2::{TokenStream, TokenTree};
use quote::ToTokens;
use syn::{parse_file, parse_str, LitStr};

#[derive(Parser)]
#[command(
    version,
    about = gettext("xgettext - extract gettext call strings from C-language source files (DEVELOPMENT)"),
    help_template = gettext("{about}\n\nUsage: {usage}\n\nArguments:\n{positionals}\n\nOptions:\n{options}"),
    disable_help_flag = true,
    disable_version_flag = true,
)]
struct Args {
    #[arg(
        short,
        help = gettext("Extract all strings, not just those found in calls to gettext family functions. Only one dot-po file shall be created")
    )]
    all: bool,

    #[arg(
        short,
        default_value_t = String::from("messages"),
        help = gettext("Name the default output file DEFAULT_DOMAIN.po instead of messages.po")
    )]
    default_domain: String,

    #[arg(
        short,
        help = gettext("\
            Join messages from C-language source files with existing dot-po files. For each dot-po file that xgettext writes messages to, \
            if the file does not exist, it shall be created. New messages shall be appended but any subsections with duplicate msgid values \
            except the first (including msgid values found in an existing dot-po file) shall either be commented out or omitted \
            in the resulting dot-po file; if omitted, a warning message may be written to standard error. Domain directives in the existing \
            dot-po files shall be ignored; the assumption is that all previous msgid values belong to the same domain. The behavior \
            is unspecified if an existing dot-po file was not created by xgettext or has been modified by another application.")
    )]
    join: bool,

    #[arg(
        short = 'K',
        help = gettext("\
            Specify an additional keyword to be looked for:\n\
            * If KEYWORD_SPEC is an empty string, this shall disable the use of default keywords for the gettext family of functions.\n\
            * If KEYWORD_SPEC is a C identifier, xgettext shall look for strings in the first argument of each call to the function or macro KEYWORD_SPEC.\n\
            * If KEYWORD_SPEC is of the form id:argnum then xgettext shall treat the argnum-th argument of a call to the function or macro id as the msgid argument, \
            where argnum 1 is the first argument.\n\
            * If KEYWORD_SPEC is of the form id:argnum1,argnum2 then xgettext shall treat strings in the argnum1-th argument \
            and in the argnum2-th argument of a call to the function or macro id as the msgid and msgid_plural arguments, respectively.")
    )]
    keyword_spec: Vec<String>,

    #[arg(
        short,
        help = gettext("Add comment lines to the output file indicating pathnames and line numbers in the source files where each extracted string is encountered")
    )]
    numbers_lines: bool,

    #[arg(
        short,
        help = gettext("Create output files in the directory specified by pathname instead of in the current working directory")
    )]
    pathname: Option<PathBuf>,

    #[arg(
        short = 'X',
        help = gettext("\
            Specify a file containing strings that shall not be extracted from the input files. \
            The format of EXCLUDE_FILE is identical to that of a dot-po file. However, \
            only statements containing msgid directives in EXCLUDE_FILE shall be used. All other statements shall be ignored.")
    )]
    exclude_file: Option<PathBuf>,

    #[arg(short, long, help = gettext("Print help"), action = clap::ArgAction::HelpLong)]
    help: Option<bool>,

    #[arg(short = 'V', long, help = gettext("Print version"), action = clap::ArgAction::Version)]
    version: Option<bool>,

    #[arg(
        name = "FILE",
        trailing_var_arg = true,
        help = gettext("A pathname of an input file containing C-language source code. If '-' is specified for an instance of file, the standard input shall be used.")
    )]
    files: Vec<PathBuf>,
}

#[derive(Debug)]
// #[cfg_attr(test, derive(Debug))]
pub struct Walker {
    keywords: HashSet<String>,
    /// msgid
    messages: Vec<LitStr>,
}

impl Walker {
    pub fn new(keyword_spec: Vec<String>) -> Self {
        assert!(!keyword_spec.is_empty());
        let mut keywords = HashSet::new();
        for value in keyword_spec {
            keywords.insert(value);
        }

        Self {
            keywords,
            messages: vec![],
        }
    }

    pub fn process_c_file(&mut self, _path: PathBuf) -> std::io::Result<()> {
        todo!();
    }

    pub fn process_rust_file(&mut self, path: PathBuf) -> std::io::Result<()> {
        let content = read_to_string(path)?;
        // TODO remove unwrap()
        let file = parse_file(&content).unwrap();
        self.walk(file.into_token_stream());
        Ok(())
    }

    fn walk(&mut self, stream: TokenStream) {
        let mut iter = stream.into_iter().peekable();
        while let Some(token) = iter.next() {
            match token {
                TokenTree::Group(group) => {
                    // going into recursion
                    self.walk(group.stream());
                }
                TokenTree::Ident(ident) => {
                    if self.keywords.contains(&ident.to_string()) {
                        if let Some(TokenTree::Group(group)) = iter.peek() {
                            if let Some(literal) = Self::extract(group.stream()) {
                                // TODO filename
                                self.messages.push(literal);
                            }
                            let _ = iter.next();
                        }
                    }
                }
                _ => {}
            }
        }
    }

    // literal string only
    fn extract(stream: TokenStream) -> Option<LitStr> {
        let mut iter = stream.into_iter().peekable();
        if let Some(TokenTree::Literal(literal)) = iter.next() {
            return parse_str(&literal.to_string()).ok();
        }
        None
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    #[cfg(debug_assertions)]
    bindtextdomain(env!("PROJECT_NAME"), "locale")?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let mut args = Args::parse();

    if args.files.is_empty() {
        eprintln!("xgettext: {}", gettext("no input file given"));
        exit(1);
    }

    // TODO remove this
    args.keyword_spec.push("gettext".into());

    let mut walker = Walker::new(args.keyword_spec);

    for path in args.files {
        match path.extension().and_then(OsStr::to_str) {
            Some("c") => walker.process_c_file(path)?,
            Some("rs") => walker.process_rust_file(path)?,
            _ => todo!(),
        }
    }

    // TODO walker.to_file()
    dbg!(&walker);

    exit(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    use quote::quote;

    #[ignore]
    #[test]
    fn test_walk() {
        let stream = quote! {
            fn main() {
                assert_eq!("Hello, world!", gettext("Hello, world!"));
            }
        };

        let mut walker = Walker::new(vec!["gettext".into()]);
        walker.walk(stream);
        // assert_eq!(walker.messages, vec!["Hello, world!".into()]);
    }
}
