//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, BufRead};
use std::path::PathBuf;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::input_stream;
use topological_sort::TopologicalSort;

/// tsort - topological sort
#[derive(Parser)]
#[command(version, about = gettext("tsort - topological sort"))]
struct Args {
    /// File to read as input.
    file: Option<PathBuf>,
}

fn tsort_file(pathname: &Option<PathBuf>) -> io::Result<i32> {
    // Handle stdin with "-" or no argument
    let file = match pathname {
        Some(path) => input_stream(path, true)?,
        None => input_stream(&PathBuf::new(), false)?,
    };
    let mut reader = io::BufReader::new(file);

    let mut ts = TopologicalSort::<String>::new();
    let mut sv: Vec<String> = Vec::new();
    let mut all_items: std::collections::HashSet<String> = std::collections::HashSet::new();

    loop {
        let mut buffer = String::new();
        let n_read = reader.read_line(&mut buffer)?;
        if n_read == 0 {
            break;
        }

        for token in buffer.split_whitespace() {
            sv.push(String::from(token));

            if sv.len() == 2 {
                all_items.insert(sv[0].clone());
                all_items.insert(sv[1].clone());

                if sv[0] == sv[1] {
                    ts.insert(String::from(&sv[0]));
                } else {
                    ts.add_dependency(String::from(&sv[0]), String::from(&sv[1]));
                }
                sv.clear();
            }
        }
    }

    // Check for odd number of tokens
    if !sv.is_empty() {
        eprintln!(
            "{}: input contains an odd number of tokens",
            pathname_display(pathname)
        );
        return Ok(1);
    }

    // Collect results and check for cycles
    let mut sorted_items = Vec::new();
    let mut sorted_set = std::collections::HashSet::new();

    for s in &mut ts {
        sorted_set.insert(s.clone());
        sorted_items.push(s);
    }

    // If there are remaining items after iteration, there's a cycle
    if ts.len() > 0 {
        eprintln!("{}: input contains a loop:", pathname_display(pathname));

        // Find items that weren't sorted (these are in the cycle)
        let mut cycle_items: Vec<String> = all_items.difference(&sorted_set).cloned().collect();
        cycle_items.sort(); // For consistent output

        // Print cycle items
        for item in &cycle_items {
            eprintln!("{}: {}", pathname_display(pathname), item);
        }

        // Print the sorted items first
        for s in sorted_items {
            println!("{}", s);
        }

        // Then print the cycle items
        for item in &cycle_items {
            println!("{}", item);
        }

        return Ok(1);
    }

    // Print results
    for s in sorted_items {
        println!("{}", s);
    }

    Ok(0)
}

fn pathname_display(path: &Option<PathBuf>) -> String {
    match path {
        None => String::from("stdin"),
        Some(p) => p.display().to_string(),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let exit_code = match tsort_file(&args.file) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("{}: {}", pathname_display(&args.file), e);
            1
        }
    };

    std::process::exit(exit_code)
}
