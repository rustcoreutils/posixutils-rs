//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::io::{self, BufRead, Write};
use std::path::PathBuf;

use clap::Parser;
use gettextrs::gettext;
use plib::io::input_stream_dashed;
use std::path::Path;

/// tsort - topological sort
#[derive(Parser)]
#[command(version, about = gettext("tsort - topological sort"))]
struct Args {
    #[arg(short = 'w', help = gettext("Set the exit status to the number of cycles found in the input"))]
    cycle_count_status: bool,

    #[arg(help = gettext("File to read as input"))]
    file: Option<PathBuf>,
}

/// Implementation-defined maximum reported by `-w` (process exit codes are
/// limited to 0..=255).
const MAX_CYCLE_STATUS: usize = 255;

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

    /// Queue every not-yet-emitted node whose in-degree is currently zero.
    fn ready_nodes(&self) -> VecDeque<String> {
        self.in_degree
            .iter()
            .filter(|(_, &deg)| deg == 0)
            .map(|(node, _)| node.clone())
            .collect()
    }

    /// Find a cycle among the remaining (not-yet-emitted) nodes via DFS,
    /// returning the nodes that form it in order. There is guaranteed to be one
    /// when this is called (Kahn's algorithm has stalled with nodes left).
    fn find_cycle(&self) -> Vec<String> {
        let mut visited: BTreeSet<String> = BTreeSet::new();
        for start in self.in_degree.keys() {
            let mut path: Vec<String> = Vec::new();
            let mut on_path: BTreeSet<String> = BTreeSet::new();
            if let Some(cycle) = self.dfs_cycle(start, &mut path, &mut on_path, &mut visited) {
                return cycle;
            }
        }
        // Unreachable in practice; degrade gracefully.
        self.in_degree.keys().next().cloned().into_iter().collect()
    }

    fn dfs_cycle(
        &self,
        node: &str,
        path: &mut Vec<String>,
        on_path: &mut BTreeSet<String>,
        visited: &mut BTreeSet<String>,
    ) -> Option<Vec<String>> {
        path.push(node.to_string());
        on_path.insert(node.to_string());

        if let Some(succs) = self.successors.get(node) {
            for succ in succs {
                // Ignore edges to already-emitted nodes.
                if !self.in_degree.contains_key(succ) {
                    continue;
                }
                if on_path.contains(succ) {
                    let idx = path.iter().position(|n| n == succ).unwrap();
                    return Some(path[idx..].to_vec());
                }
                if !visited.contains(succ) {
                    if let Some(cycle) = self.dfs_cycle(succ, path, on_path, visited) {
                        return Some(cycle);
                    }
                }
            }
        }

        path.pop();
        on_path.remove(node);
        visited.insert(node.to_string());
        None
    }

    /// Produce a total order. When the partial order contains cycles they are
    /// detected and broken (one edge each) so output still covers every node;
    /// each broken cycle is returned so the caller can report and count it.
    fn sort(mut self) -> (Vec<String>, Vec<Vec<String>>) {
        let mut result = Vec::with_capacity(self.in_degree.len());
        let mut cycles: Vec<Vec<String>> = Vec::new();
        let mut queue = self.ready_nodes();

        loop {
            // Emit everything currently free of dependencies (Kahn's algorithm).
            while let Some(node) = queue.pop_front() {
                result.push(node.clone());
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

            if self.in_degree.is_empty() {
                break;
            }

            // Stalled: a cycle remains. Find it, break the edge that closes it,
            // and continue; the freed nodes are re-queued below.
            let cycle = self.find_cycle();
            let from = cycle.last().unwrap().clone();
            let to = cycle.first().unwrap().clone();
            if let Some(succs) = self.successors.get_mut(&from) {
                if succs.remove(&to) {
                    if let Some(deg) = self.in_degree.get_mut(&to) {
                        *deg -= 1;
                    }
                }
            }
            cycles.push(cycle);
            queue = self.ready_nodes();
        }

        (result, cycles)
    }
}

fn tsort_file(pathname: &Option<PathBuf>) -> io::Result<usize> {
    // No operand or "-" reads stdin; any other path is opened as a file.
    let file = input_stream_dashed(pathname.as_deref().unwrap_or(Path::new("")))?;
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

    // The application is responsible for supplying token pairs; an odd count
    // is advisory (the last token is ignored).
    if !sv.is_empty() {
        plib::diag::warning(&gettext("odd number of tokens"));
    }

    // Perform the sort, breaking and reporting any cycles.
    let (sorted, cycles) = ts.sort();

    // Output the total order to stdout (cycle members are included, since their
    // cycles were broken).
    let mut stdout = io::stdout().lock();
    for node in &sorted {
        writeln!(stdout, "{}", node)?;
    }

    // Report each broken cycle to stderr; reporting makes the exit status
    // non-zero (unless -w overrides it with the cycle count).
    for cycle in &cycles {
        plib::diag::error(&gettext("cycle in data"));
        for node in cycle {
            plib::diag::error(node);
        }
    }

    Ok(cycles.len())
}

fn pathname_display(path: &Option<PathBuf>) -> String {
    match path {
        None => String::from("stdin"),
        Some(p) => p.display().to_string(),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("tsort");

    let args = Args::parse();

    match tsort_file(&args.file) {
        Ok(cycle_count) => {
            if args.cycle_count_status {
                // -w: exit status is the number of cycles, capped at the
                // implementation-defined maximum.
                std::process::exit(cycle_count.min(MAX_CYCLE_STATUS) as i32);
            }
            // Otherwise, a reported cycle makes the status non-zero.
            std::process::exit(plib::diag::exit_status());
        }
        Err(e) => {
            plib::diag::error(&format!("{}: {}", pathname_display(&args.file), e));
            std::process::exit(1);
        }
    }
}
