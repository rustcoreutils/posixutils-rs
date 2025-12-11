use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use std::io::{self, BufRead, Write};
use std::path::PathBuf;

#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Convert all sequences of two or more spaces to tabs
    #[arg(short = 'a')]
    all_spaces: bool,

    /// Specify tab stops
    #[arg(short = 't')]
    tablist: Option<String>,

    /// Input files
    files: Vec<PathBuf>,
}

fn parse_tablist(s: &str) -> Result<Vec<usize>, std::num::ParseIntError> {
    s.split(',').map(|item| item.parse::<usize>()).collect()
}

fn unexpand(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    let tablist = match &args.tablist {
        Some(s) => parse_tablist(s)?,
        None => vec![8],
    };
    let mut stdout = io::stdout();

    if (args.files.len() == 1 && args.files[0].as_os_str() == "-") || args.files.is_empty() {
        let reader = io::stdin();
        let lines = io::BufReader::new(reader).lines();
        for line in lines {
            let line = line?;
            let converted_line = if args.all_spaces && args.tablist.is_none() {
                convert_all_blanks(&line, &tablist)
            } else {
                convert_leading_blanks(&line, &tablist)
            };
            writeln!(stdout, "{}", converted_line)?;
        }
    } else {
        for file in &args.files {
            let reader = io::BufReader::new(std::fs::File::open(file)?);
            for line in reader.lines() {
                let line = line?;
                let converted_line = if args.all_spaces && args.tablist.is_none() {
                    convert_all_blanks(&line, &tablist)
                } else {
                    convert_leading_blanks(&line, &tablist)
                };
                writeln!(stdout, "{}", converted_line)?;
            }
        }
    };

    Ok(())
}

fn convert_leading_blanks(line: &str, tablist: &[usize]) -> String {
    let mut result = String::new();
    let mut space_count = 0;
    let mut chars = line.chars().peekable();

    while let Some(&ch) = chars.peek() {
        if ch == ' ' {
            space_count += 1;
            chars.next();
        } else {
            break;
        }
    }

    let mut col = 0;
    for &tabstop in tablist {
        while space_count > 0 && col < tabstop {
            let spaces_to_next_tabstop = tabstop - col;
            if space_count >= spaces_to_next_tabstop {
                result.push('\t');
                space_count -= spaces_to_next_tabstop;
                col = tabstop;
            } else {
                col += space_count;
                break;
            }
        }
    }

    for _ in 0..space_count {
        result.push(' ');
    }

    result.push_str(&chars.collect::<String>());
    result
}

fn split_whitespaces(line: &str) -> Vec<String> {
    let mut parts = Vec::new();
    let mut current_part = String::new();
    let mut in_word = false;

    for c in line.chars() {
        if c.is_whitespace() {
            if in_word {
                parts.push(current_part.clone());
                current_part.clear();
                in_word = false;
            }
        } else if !in_word {
            in_word = true;
        }

        current_part.push(c);
    }

    if !current_part.is_empty() {
        parts.push(current_part);
    }
    parts
}

fn convert_all_blanks(line: &str, tablist: &[usize]) -> String {
    let mut result = String::new();

    let split_parts: Vec<String> = split_whitespaces(line);

    for part in &split_parts {
        result.push_str(&convert_spaces_to_tabs(part, tablist[0]));
    }

    result
}

fn convert_spaces_to_tabs(line: &str, tabstop: usize) -> String {
    let mut result = String::new();
    let mut space_count = 0;
    let mut chars = line.chars().peekable();

    while let Some(&ch) = chars.peek() {
        if ch == ' ' {
            space_count += 1;
            chars.next();
        } else {
            break;
        }
    }

    while space_count > 0 {
        if space_count >= tabstop {
            result.push('\t');
            space_count -= tabstop;
        } else {
            break;
        }
    }

    for _ in 0..space_count {
        result.push(' ');
    }

    result.push_str(&chars.collect::<String>());
    result
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    if let Err(err) = unexpand(&args) {
        exit_code = 1;
        eprintln!("{}", err);
    }

    std::process::exit(exit_code)
}
