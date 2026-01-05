//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::io::{self, BufRead};
use std::path::PathBuf;

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use plib::io::input_stream_opt;

/// tsort - topological sort
#[derive(Parser)]
#[command(version, about = gettext("tsort - topological sort"))]
struct Args {
    #[arg(help = gettext("File to read as input"))]
    file: Option<PathBuf>,
}

/// Topological sort using Kahn's algorithm
struct TopoSort {
    /// Adjacency list: node -> set of successors (BTreeMap/BTreeSet for deterministic order)
    successors: BTreeMap<String, BTreeSet<String>>,
    /// In-degree count for each node
    in_degree: BTreeMap<String, usize>,
}

impl TopoSort {
    fn new() -> Self {
        TopoSort {
            successors: BTreeMap::new(),
            in_degree: BTreeMap::new(),
        }
    }

    /// Add a node (ensures it exists even with no edges)
    fn add_node(&mut self, node: String) {
        // Clone first for in_degree, then move to successors (reduces one clone)
        self.in_degree.entry(node.clone()).or_insert(0);
        self.successors.entry(node).or_default();
    }

    /// Add a dependency: `from` must come before `to`
    fn add_dependency(&mut self, from: String, to: String) {
        // Ensure both nodes exist
        self.add_node(from.clone());
        self.add_node(to.clone());

        // Add edge from -> to (only if not already present)
        if self.successors.get_mut(&from).unwrap().insert(to.clone()) {
            // Increment in-degree of 'to' only if this is a new edge
            *self.in_degree.get_mut(&to).unwrap() += 1;
        }
    }

    /// Perform topological sort, returning (sorted_nodes, cycle_nodes)
    fn sort(mut self) -> (Vec<String>, Vec<String>) {
        let node_count = self.in_degree.len();
        let mut result = Vec::with_capacity(node_count);
        let mut queue: VecDeque<String> = VecDeque::new();

        // Find all nodes with in-degree 0
        for (node, &deg) in &self.in_degree {
            if deg == 0 {
                queue.push_back(node.clone());
            }
        }

        // Process nodes with no remaining dependencies
        while let Some(node) = queue.pop_front() {
            result.push(node.clone());

            // Remove this node's edges and update in-degrees
            if let Some(succs) = self.successors.remove(&node) {
                for succ in succs {
                    if let Some(deg) = self.in_degree.get_mut(&succ) {
                        *deg -= 1;
                        if *deg == 0 {
                            queue.push_back(succ);
                        }
                    }
                }
            }
            self.in_degree.remove(&node);
        }

        // Any remaining nodes are in cycles
        let mut cycle_nodes = Vec::with_capacity(self.in_degree.len());
        cycle_nodes.extend(self.in_degree.into_keys());

        (result, cycle_nodes)
    }
}

fn tsort_file(pathname: &Option<PathBuf>) -> io::Result<()> {
    let file = input_stream_opt(pathname)?;
    let reader = io::BufReader::new(file);

    let mut ts = TopoSort::new();
    let mut sv: Vec<String> = Vec::with_capacity(2);

    for line in reader.lines() {
        for token in line?.split_whitespace() {
            sv.push(String::from(token));

            if sv.len() == 2 {
                if sv[0] == sv[1] {
                    // Self-pair: just add the node (presence, not ordering)
                    ts.add_node(sv[0].clone());
                } else {
                    // Different pair: add dependency (sv[0] before sv[1])
                    ts.add_dependency(sv[0].clone(), sv[1].clone());
                }
                sv.clear();
            }
        }
    }

    // Warn if odd number of tokens (last token silently discarded)
    if !sv.is_empty() {
        eprintln!("{}", gettext("tsort: odd number of tokens"));
    }

    // Perform the sort
    let (sorted, cycle_nodes) = ts.sort();

    // Output sorted nodes
    for node in &sorted {
        println!("{}", node);
    }

    // Handle cycles
    if !cycle_nodes.is_empty() {
        eprintln!("{}", gettext("tsort: cycle in data"));

        // Report cycle members to stderr (match macOS behavior)
        for node in &cycle_nodes {
            eprintln!("tsort: {}", node);
        }

        // Output cycle members to stdout (match macOS behavior)
        for node in &cycle_nodes {
            println!("{}", node);
        }
    }

    Ok(())
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

    let mut exit_code = 0;

    if let Err(e) = tsort_file(&args.file) {
        exit_code = 1;
        eprintln!("{}: {}", pathname_display(&args.file), e);
    }

    std::process::exit(exit_code)
}
