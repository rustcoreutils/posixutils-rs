//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, BufWriter, Read, Write};
use std::path::PathBuf;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::input_stream;
use plib::BUFSZ;

/// expand - convert tabs to spaces
#[derive(Parser)]
#[command(version, about = gettext("expand - convert tabs to spaces"))]
struct Args {
    #[arg(short, long, help = gettext("Tab stops, either a single positive decimal integer or a list of tabstops separated by commas."))]
    tablist: Option<String>,

    #[arg(help = gettext("Files to read as input."))]
    files: Vec<PathBuf>,
}

enum TabList {
    UniStop(usize),
    Stops(Vec<usize>),
}

fn parse_tablist(tablist: &str) -> Result<TabList, &'static str> {
    let res = tablist.parse::<usize>();
    if let Ok(tab) = res {
        return Ok(TabList::UniStop(tab));
    }

    let mut v = Vec::new();
    for token in tablist.split(&[' ', ','][..]) {
        let n = match token.parse::<usize>() {
            Ok(val) => val,
            Err(_e) => return Err("Invalid tab stop in list"),
        };

        if !v.is_empty() {
            let last = *v.iter().last().unwrap();
            if n <= last {
                return Err("Invalid tab stop order in list");
            }
        }

        v.push(n);
    }

    Ok(TabList::Stops(v))
}

fn space_out(column: &mut usize, writer: &mut BufWriter<dyn Write>) -> io::Result<()> {
    *column += 1;

    writer.write_all(b" ")?;

    Ok(())
}

fn expand_file(tablist: &TabList, pathname: &PathBuf) -> io::Result<()> {
    // open file, or stdin
    let mut file = input_stream(pathname, false)?;

    let mut raw_buffer = [0; BUFSZ];
    let mut writer = BufWriter::new(io::stdout());
    let mut column: usize = 1;
    let mut cur_stop = 0;

    loop {
        // read a chunk of file data
        let n_read = file.read(&mut raw_buffer[..])?;
        if n_read == 0 {
            break;
        }

        // slice of buffer containing file data
        let buf = &raw_buffer[0..n_read];

        for byte_ref in buf {
            let byte = *byte_ref;
            if byte == 0x8 {
                // backspace
                writer.write_all(&[byte])?;
                if column > 1 {
                    column -= 1;
                }
            } else if byte == b'\r' || byte == b'\n' {
                writer.write_all(&[byte])?;
                column = 1;
            } else if byte != b'\t' {
                writer.write_all(&[byte])?;
                column += 1;
            } else {
                match tablist {
                    TabList::UniStop(n) => {
                        while (column % n) != 0 {
                            space_out(&mut column, &mut writer)?;
                        }
                        space_out(&mut column, &mut writer)?;
                    }
                    TabList::Stops(tabvec) => {
                        let last_tab: usize = tabvec[tabvec.len() - 1];
                        let next_tab = tabvec[cur_stop];

                        if column >= last_tab {
                            space_out(&mut column, &mut writer)?;
                        } else {
                            while column < next_tab {
                                space_out(&mut column, &mut writer)?;
                            }
                            cur_stop += 1;
                            space_out(&mut column, &mut writer)?;
                        }
                    }
                }
            }
        }
    }

    writer.flush()?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    let tablist = {
        if let Some(ref tablist) = args.tablist {
            match parse_tablist(tablist) {
                Ok(tl) => tl,
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(1);
                }
            }
        } else {
            TabList::UniStop(8)
        }
    };

    // if no files, read from stdin
    if args.files.is_empty() {
        args.files.push(PathBuf::new());
    }

    let mut exit_code = 0;

    for filename in &args.files {
        if let Err(e) = expand_file(&tablist, filename) {
            exit_code = 1;
            eprintln!("{}: {}", filename.display(), e);
        }
    }

    std::process::exit(exit_code)
}
